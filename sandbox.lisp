;;;; sandbox.lisp
;; Copyright © 2014 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see License.txt (MIT License)

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
                         'sandbox-default-readtable))
   
   (package :accessor sandbox-package :initarg :package)
   (symbols :accessor sandbox-symbols :initform (make-hash-table))
   (packages :accessor sandbox-packages
             :initform (make-hash-table :test 'equal))
   (shadow-packages :accessor shadow-packages
                    :initform (make-hash-table :test 'equal)))
  (:default-initargs
   :name (gensym *sandbox-name-default*)
   :crate (make-instance 'crate:crate)))

(defmethod print-object ((object sandbox) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (name) object
      (format stream "name:~a" name))))

(defun get-shadow-package (sandbox package)
  (gethash (package-name package)
           (shadow-packages sandbox)))

(defun symbol-shadow-package (sandbox symbol)
  (get-shadow-package sandbox (symbol-package symbol)))

(defun symbol-shadow-symbol (sandbox symbol)
  (find-symbol (symbol-name symbol)
               (symbol-shadow-package sandbox symbol)))

 (defun symbol-in-sandbox-p (sandbox symbol)
     (if (gethash (package-name (symbol-package symbol))
                  (sandbox-packages sandbox))
         t
         nil))

(defun user-package-name (sandbox package)
  (loop for v being the hash-values in (shadow-packages sandbox)
          using (hash-key k)
        do (when (eq v package)
             (return k))
        finally (return (package-name package))))

(defun shadowed-package (sandbox package)
  (loop for v being the hash-values in (shadow-packages sandbox)
          using (hash-key k)
        do (when (eq v package)
             (return (find-package k)))
        finally (return package)))

;; stolen from swank::package-prompt
(defun shortest-package-name (package)
  (reduce (lambda (x y) (if (<= (length x) (length y)) x y))
	  (cons (package-name package) (package-nicknames package))))


(defun box-echo-symbol (sandbox symbol)
  (crate:promote-inferior-symbol (sandbox-crate sandbox) symbol))

(defun box-shadow-symbol (sandbox symbol &optional alternative-package)
  (crate:shadow-external-symbol (sandbox-crate sandbox)
                                symbol alternative-package))

(defun box-out-symbol (sandbox symbol)
  (let* ((sym-name (symbol-name symbol))
         (new-sym (crate:promote-inferior-symbol (sandbox-crate sandbox) symbol))
         (inferior-sym (crate:symbol-inferior new-sym)))
    (if (macro-function symbol)
        (setf (macro-function inferior-sym)
              (lambda (&rest junk)
                (declare (ignore junk))
                (error 'boxed-out-macro :name sym-name)))
        (if (fboundp symbol)
            (setf (symbol-function inferior-sym)
                  (lambda (&rest junk)
                    (declare (ignore junk))
                    (error 'boxed-out-function :name sym-name)))))
    (eval `(defsetf ,inferior-sym (&rest junk) ()
             (declare (ignore junk))
             `(error 'boxed-out-setf :name ,,sym-name)))))

(defun box-redefine-symbol (sandbox symbol definer)
  (let* ((new-sym (crate:promote-inferior-symbol (sandbox-crate sandbox) symbol))
         (inferior-sym (crate:symbol-inferior new-sym)))
    (funcall definer sandbox symbol inferior-sym)))

(defun box-ignore-symbol (sandbox symbol)
  (declare (ignore sandbox symbol))
  nil)

(defun symbol-status (sandbox symbol)
  (gethash symbol (sandbox-symbols sandbox)))

(defun symbol-locked-p (sandbox symbol)
  (eq (symbol-status sandbox symbol)
      :boxed-out))

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

(defun box-in-form (sandbox form)
  (when (and (consp form) (circular-tree-p form))
    (error 'circular-lisp-form))
  (let ((cons-count 0))
    (labels
        ((convert-cons (form)
           (if (> (incf cons-count) *max-form-size*)
               (error 'box-form-dimension-error
                      :type (type-of form)
                      :actual-dimension cons-count
                      :max-dimension *max-form-size*)
               (cons (convert (car form)) (convert (cdr form)))))
         (convert-array (form)
           (let ((dim (reduce #'* (array-dimensions form))))
             (when (> dim *max-form-size*)
               (error 'box-form-dimension-error
                      :type (type-of form)
                      :actual-dimension dim
                      :max-dimension *max-form-size*))
             (let* ((eltype (array-element-type form))
                    (new-array (make-array (array-dimensions form)
                                           :element-type eltype))
                    (orig-displaced (make-array dim :displaced-to form
                                                    :element-type eltype))
                    (new-displaced (make-array dim :displaced-to new-array
                                                   :element-type eltype)))
               ;; map-into
               (map-into new-displaced
                         (curry #'box-in-form sandbox)
                         orig-displaced)
               ;; original eval bot version
               #+(or)
               (dotimes (i (array-total-size new-array))
                 (setf (row-major-aref new-array i)
                       (box-in-form sandbox
                                    (row-major-aref form i))))
               new-array)))
         (convert-symbol (form)
           (if (symbol-in-sandbox-p sandbox form)
               (if (equal (symbol-name form) "NIL")
                   (symbol-shadow-symbol sandbox cl::nil) ;; hack
                   form)
               (let ((pkg (or (symbol-shadow-package sandbox form)
                              #|(make-shadow-package sandbox
                                                   (symbol-package form))|#)))
                 (intern (symbol-name form) pkg))))
         (convert (form)
           (typecase form
             (null form)
             (number form)
             (character form)
             (pathname form)
             (keyword form)
             (cons (convert-cons form))
             (array (convert-array form))
             (symbol (convert-symbol form))
             (t (error 'box-unsupported-type :type (type-of form))))))
      (convert form))))


(defun sandbox-value (sandbox symbol)
  (symbol-value (symbol-shadow-symbol sandbox symbol)))

(defun (setf sandbox-value) (value sandbox symbol)
  (setf (symbol-value (symbol-shadow-symbol sandbox symbol))
        value))

(defun quit ()
  (signal 'repl-quit-signal))

(defun repl-read (sandbox stream)
  (let ((*readtable* (sandbox-readtable sandbox)))
    (box-in-form sandbox
                (handler-case (read stream)
                  (end-of-file () (signal 'repl-read-done))))))

(defun repl-read-string (sandbox string)
  (let ((*package* (sandbox-package sandbox)))
    (with-input-from-string (s string)
            (repl-read sandbox s))))

(defun repl-eval (sandbox form &key timeout)
  (declare (ignore sandbox))
  #|(setf (sandbox-value sandbox 'cl:-) form)|#
  (trivial-timeout:with-timeout (timeout)
    (eval form)))

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
        (*package* (sandbox-package sandbox))
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

(defun repl (sandbox &key timeout)
  (let ((*package* (sandbox-package sandbox))
        (*readtable* (sandbox-readtable sandbox)))
    (loop
      (clear-input)
      (format t  "~&~a>> " (shortest-package-name
                           (shadowed-package sandbox
                                             (sandbox-package sandbox))))
      (handler-case
          (repl-print sandbox
                      (multiple-value-list
                       (repl-stream sandbox *standard-input*
                                    :output-stream *standard-output*
                                    :timeout timeout :loop nil))
                      *standard-output*)
        (repl-quit-signal ()
          (return "Bye"))))))
