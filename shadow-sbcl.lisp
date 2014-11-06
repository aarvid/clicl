;;;; shadow-sbcl.lisp
;; Copyright © 2014 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see License.txt


(in-package #:clicl)

#+sbcl
(defun redefine-defmacro (sandbox old-symbol new-symbol)
  (declare (ignore old-symbol))
  (sb-c::%defmacro
   new-symbol
   (lambda (whole env)
     (declare (ignore env))
     (let ((args (cdr whole)))
       (unless
           (sb-int:list-of-length-at-least-p args 2)
         (sb-kernel::arg-count-error 'defmacro new-symbol
                                     args
                                     '(name lambda-list &body body)
                                     2 nil)))
     (let* ((name (car (cdr whole)))
            (lambda-list (car (cdr (cdr whole))))
            (body (cdr (cdr (cdr whole)))))
       (when (symbol-locked-p sandbox name)
         (error 'box-locked-symbol :name name))
       `(cl:defmacro ,name ,lambda-list ,@body)))
   '(name lambda-list &body body)
   nil
   `(macro-function ,new-symbol)
   (sb-c:source-location))
  new-symbol)

#+sbcl
(defun redefine-defun (sandbox old-symbol new-symbol)
  (declare (ignore old-symbol))
  (sb-c::%defmacro
   new-symbol
   (lambda (whole env)
     (declare (ignore env))
     (let ((args (cdr whole)))
       (unless
           (sb-int:list-of-length-at-least-p args 2)
         (sb-kernel::arg-count-error 'defmacro new-symbol
                                     args
                                     '(name lambda-list &body body)
                                     2 nil)))
     (let* ((name (car (cdr whole)))
            (lambda-list (car (cdr (cdr whole))))
            (body (cdr (cdr (cdr whole))))
            (fsymbol (cond ((symbolp name) name)
                           ((and (consp name)
                                 (eq (car name) 'setf))
                            (cadr name)))))
       (when (and fsymbol (symbol-locked-p sandbox fsymbol))
         (error 'box-locked-symbol :name fsymbol))
       `(cl:defun ,name ,lambda-list ,@body)))
   '(name lambda-list &body body)
   nil
   `(macro-function ,new-symbol)
   (sb-c:source-location))
  new-symbol)

;;;#+sbcl
(defparameter *shadow-sbcl-symbols*
  nil
  #|'((sb-impl::backq-list :shadow)
    (sb-impl::backq-list* :shadow)
    (sb-impl::backq-append :shadow)
    (sb-impl::backq-append :shadow)
    (sb-impl::backq-nconc :shadow)
    (sb-impl::backq-vector :shadow))|#)
