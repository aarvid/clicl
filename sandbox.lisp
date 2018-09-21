;;;; sandbox.lisp
;; Copyright © 2014-2015 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see LICENSE.txt (BSD-2 License)

(in-package #:clicl)



(defvar *sandbox-name-default* "SANDBOX")

(defvar *sandbox* nil)

(defun sharp-illegal (stream sub-char arg)
  (declare (ignore stream arg))
  (error  'illegal-sharp-reader-macro :schar sub-char))

;; the default read table for a sandbox.
;; avoid read time evaluations and other possible harmful acts.
(named-readtables:defreadtable sandbox-default-readtable
  (:merge :standard)
  (:dispatch-macro-char #\# #\. #'sharp-illegal)  ; no evaluation
  (:dispatch-macro-char #\# #\+ #'sharp-illegal)  ; no conditionals
  (:dispatch-macro-char #\# #\- #'sharp-illegal)  ; no conditionals
  (:dispatch-macro-char #\# #\= #'sharp-illegal)  ; no labels
  (:dispatch-macro-char #\# #\# #'sharp-illegal)) ; no label references.




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

(defmacro with-sandbox ((var-or-sandbox
                          &optional (sandbox-if-var nil sandbox-if-var-supplied-p))
                         &body body)
  (let ((var (if sandbox-if-var-supplied-p var-or-sandbox (gensym)))
        (sandbox (if sandbox-if-var-supplied-p sandbox-if-var var-or-sandbox)))
   `(let ((,var ,sandbox))
      (let ((*sandbox* ,var)
            (*readtable* (sandbox-readtable ,var)))
        (crate:with-crate ((sandbox-crate ,var))
          ,@body)))))

(defun repl-read* (stream)
  (handler-case (clicl-reader:read stream)
        (end-of-file () (signal 'repl-read-done))))

(defun repl-read (sandbox stream)
  (with-sandbox (sandbox)
    (repl-read* stream)))

(defun repl-read-string (sandbox string)
  (with-input-from-string (s string)
    (repl-read sandbox s)))

(defun repl-eval* (form &key timeout)
  (setf (sandbox-value *sandbox* 'cl:-) form)
  (trivial-timeout:with-timeout (timeout)
    (eval form)))

(defun repl-eval (sandbox form &key timeout)
  (with-sandbox (sandbox)
    (repl-eval* form :timeout timeout)))

(defun repl-print* (values stream)
  (if values
      (clicl-printer:format stream "~{~s~% ~}" values)
      (format stream "; No value~%")))

(defun repl-print (sandbox values stream)
  (with-sandbox (sandbox)
    (repl-print* values stream)))


(defun repl-stream* (input-stream
                    &key (output-stream *standard-output*)
                         timeout loop (repl-vars? t)
                         (output-values? t))
  (let ((vals)
        (*standard-output* output-stream))
    (handler-case
        (handler-bind ((warning (lambda (c)
                                  (when (find-restart 'muffle-warning c)
                                    (muffle-warning)))))
          (tagbody top
             (setf vals
                   (multiple-value-list
                    (repl-eval* (repl-read* input-stream) :timeout timeout)))
             (when loop (go top))))
      (repl-read-done ()
        (when output-values?
         (repl-print* vals output-stream))))
    (when repl-vars?
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

(defun repl-stream (sandbox input-stream &rest keys
                    &key (output-stream *standard-output*)
                         timeout loop (repl-vars? t)
                         (output-values? t))
  (declare (ignore output-stream timeout loop repl-vars? output-values?))
  (with-sandbox (sandbox)
    (apply #'repl-stream* input-stream keys )))

(defun repl-string (sandbox string
                    &optional (output-stream *standard-output*) timeout (output-values? t))
  (with-input-from-string (input-stream string)
    (repl-stream sandbox input-stream :output-stream output-stream
                                      :timeout timeout :loop t
                                      :output-values? output-values?)))

(defun shortest-name-current-package (sandbox)
  (crate:with-crate ((sandbox-crate sandbox))
    (shortest-package-name (crate:current-package))))


(defun repl (sandbox &key timeout)
  (with-sandbox (sandbox)
   (loop
     (clear-input)
     (format t  "~&~a>> " (shortest-name-current-package sandbox))
     (handler-case
         (repl-print sandbox
                     (multiple-value-list
                      (repl-stream* *standard-input*
                                   :output-stream *standard-output*
                                   :timeout timeout :loop nil))
                     *standard-output*)
       (repl-quit-signal ()
         (return "Bye"))))))


(defun load-stream (sandbox stream &key print)
  (repl-stream sandbox stream :loop t :repl-vars nil))

(defun load-string (sandbox string &key print)
  (with-input-from-string (stream string)
    (load-stream sandbox stream :print print)))

(defun load-file (sandbox file &key print)
  (with-input-from-file (stream file)
    (load-stream sandbox stream :print print)))

