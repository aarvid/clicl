;;;; sandbox.lisp
;; Copyright © 2014-2015 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see LICENSE.txt (BSD-2 License)

(in-package #:clicl)



(defvar *sandbox-name-default* "SANDBOX")
(defvar *sandbox* nil)

(defun sharp-illegal (stream sub-char arg)
  (declare (ignore stream arg))
  (error  'illegal-sharp-reader-macro :schar sub-char))

(named-readtables:defreadtable sandbox-default-readtable
  (:merge :standard)
  (:dispatch-macro-char #\# #\. #'sharp-illegal)
  (:dispatch-macro-char #\# #\+ #'sharp-illegal)
  (:dispatch-macro-char #\# #\- #'sharp-illegal)
  (:dispatch-macro-char #\# #\= #'sharp-illegal)
  (:dispatch-macro-char #\# #\# #'sharp-illegal))




(defclass sandbox ()
  ((name :accessor sandbox-name
         :initarg :name
         :initform *sandbox-name-default*)
   (crate :accessor sandbox-crate
          :initarg :crate)
   (readtable :accessor sandbox-readtable
              :initarg :readtable
              :initform (named-readtables:find-readtable
                         'sandbox-default-readtable)))
  (:default-initargs
   :name (gensym *sandbox-name-default*)
   :crate (make-instance 'crate:crate)))

(defmethod print-object ((object sandbox) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (name) object
      (format stream "name:~a" name))))



;; stolen from swank::package-prompt
(defun shortest-package-name (package)
  (reduce (lambda (x y) (if (<= (length x) (length y)) x y))
	  (cons (crate:package-name package)
                (crate:package-nicknames package))))


(defun box-echo-symbol (sandbox symbol)
  (crate:promote-inferior-symbol (sandbox-crate sandbox) symbol))

(defun box-shadow-symbol (sandbox symbol &optional alternative-package)
  (crate:shadow-external-symbol (sandbox-crate sandbox)
                                symbol alternative-package))

(defun box-out-symbol (sandbox symbol)
  (let* ((sym-name (symbol-name symbol))
         (new-sym (crate:promote-inferior-symbol (sandbox-crate sandbox) symbol)))
    (if (macro-function symbol)
        (setf (macro-function new-sym)
              (lambda (&rest junk)
                (declare (ignore junk))
                (error 'boxed-out-macro :name sym-name)))
        (if (fboundp symbol)
            (setf (symbol-function new-sym)
                  (lambda (&rest junk)
                    (declare (ignore junk))
                    (error 'boxed-out-function :name sym-name)))))
    (eval `(defsetf ,new-sym (&rest junk) ()
             (declare (ignore junk))
             `(error 'boxed-out-setf :name ,,sym-name)))))

(defun box-redefine-symbol (sandbox symbol definer)
  (let* ((new-sym (crate:promote-inferior-symbol (sandbox-crate sandbox)
                                                 symbol)))
    (funcall definer sandbox symbol new-sym)))

(defun box-ignore-symbol (sandbox symbol)
  (declare (ignore sandbox symbol))
  nil)


(defun process-symbol-treatment (sandbox symbols)
  "configure a sandbox with a list of lists with symbols and their treatment,
   each symbol is a list of the symbol, treatment and perhaps a function.
   possible treatments:
     :shadow - import symbol to sandbox, with optional package if symbol
               is to reference same symbol-name in another package
     :echo - only create symbol in sandbox
     :box-out - deny access to symbol with warning when accessed
     :ignore - deny access to symbol but with NO warning when accessed,
               user is free to use symbol in his sandbox package
               for his own usage.
     :redefine - redefine symbol, requires a function of three values
                 (sandbox old-symbol new-symbol) that redefines the symbol.
   "
  (dolist (sym-rule symbols)
    (destructuring-bind (symbol action &optional data) sym-rule
      (ecase action
        (:echo (box-echo-symbol sandbox symbol))
        (:shadow (box-shadow-symbol sandbox symbol data))
        (:box-out (box-out-symbol sandbox symbol))
        (:redefine (box-redefine-symbol sandbox symbol data))
        (:ignore (box-ignore-symbol sandbox symbol))))))

(defvar *max-form-size* 1000)

(defun sandbox-value (sandbox symbol)
  (symbol-value (crate:get-crate-symbol (sandbox-crate sandbox) symbol)))

(defun (setf sandbox-value) (value sandbox symbol)
  (setf (symbol-value (crate:get-crate-symbol (sandbox-crate sandbox) symbol))
        value))

(defun quit ()
  (signal 'repl-quit-signal))

(defun repl-read (sandbox stream)
  (let ((*readtable* (sandbox-readtable sandbox)))
    (crate:with-crate ((sandbox-crate sandbox))
      (handler-case (clicl-read:read stream)
        (end-of-file () (signal 'repl-read-done))))))

(defun repl-read-string (sandbox string)
  (with-input-from-string (s string)
    (repl-read sandbox s)))

(defun repl-eval (sandbox form &key timeout)
  ;;(declare (ignore sandbox))
  (crate:with-crate ((sandbox-crate sandbox))
   (setf (sandbox-value sandbox 'cl:-) form)
   (trivial-timeout:with-timeout (timeout)
     (eval form))))

(defun repl-print (sandbox values stream)
  (declare (ignore sandbox))
  (if values
      (format stream "~{~s~% ~}" values)
      (format stream "; No value~%")))


(defun repl-stream (sandbox input-stream
                    &key (output-stream *standard-output*)
                         timeout loop (repl-vars t))
  (let ((vals)
        (*sandbox* sandbox)
        (*readtable* (sandbox-readtable sandbox)))
    (handler-case
        (handler-bind ((warning (lambda (c)
                                  (when (find-restart 'muffle-warning c)
                                    (muffle-warning)))))
          (tagbody top
             (setf vals
                   (multiple-value-list
                    (repl-eval sandbox
                               (repl-read sandbox input-stream)
                               :timeout  timeout)))
             (when loop (go top))))
      (repl-read-done ()
        (repl-print sandbox vals output-stream)))
    (when repl-vars
      (setf (sandbox-value *sandbox* '///) (sandbox-value *sandbox* '//)
            (sandbox-value *sandbox* '//)  (sandbox-value *sandbox* '/)
            (sandbox-value *sandbox* '/)   vals
            (sandbox-value *sandbox* '***) (sandbox-value *sandbox* '**)
            (sandbox-value *sandbox* '**)  (sandbox-value *sandbox* '*)
            (sandbox-value *sandbox* '*)   (car vals)
            (sandbox-value *sandbox* '+++) (sandbox-value *sandbox* '++)
            (sandbox-value *sandbox* '++)  (sandbox-value *sandbox* '+)
            (sandbox-value *sandbox* '+)   (sandbox-value *sandbox* '-)))
    (values-list vals)))

(defun repl-string (sandbox string
                    &optional (output-stream *standard-output*) timeout)
  (with-input-from-string (input-stream string)
    (repl-stream sandbox input-stream :output-stream output-stream
                                      :timeout timeout :loop t)))

(defun shortest-name-current-package (sandbox)
  (crate:with-crate ((sandbox-crate sandbox))
    (shortest-package-name (crate:current-package))))

(defun repl (sandbox &key timeout)
  (loop
    (clear-input)
    (format t  "~&~a>> " (shortest-name-current-package sandbox))
    (handler-case
        (repl-print sandbox
                    (multiple-value-list
                     (repl-stream sandbox *standard-input*
                                  :output-stream *standard-output*
                                  :timeout timeout :loop nil))
                    *standard-output*)
      (repl-quit-signal ()
        (return "Bye")))))
