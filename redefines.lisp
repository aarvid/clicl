(cl:in-package :cl-user)

(cl:defpackage #:clicl-redefines
  (:use #:cl #:alexandria)
  (:shadow #:defun)
  (:export #:defun
           ))


(cl:defmacro clicl-redefines:defun (name lambda-list cl:&body body)
  (crate:check-can-defun-symbol name)
  `(progn
     (cl:defun ,name ,lambda-list ,@body)))
