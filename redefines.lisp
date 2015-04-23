(cl:in-package :cl-user)

(cl:defpackage #:clicl-redefines
  (:use #:cl #:alexandria)
  (:shadow #:defun
           #:defmacro)
  (:export #:defun
           #:defmacro
           ))

;;; this package/file contains macros and functions for the sandbox
;;; that are redefined.

(cl:defmacro clicl-redefines:defun (name lambda-list cl:&body body)
  (crate:check-can-defun-symbol name)
  `(progn
     (cl:defun ,name ,lambda-list ,@body)))

(cl:defmacro clicl-redefines:defmacro (name lambda-list cl:&body body)
  (crate:check-can-macro-symbol name)
  `(progn
     (cl:defmacro ,name ,lambda-list ,@body)))

;; defgeneric, defmethod

(cl:defmacro clicl-redefines::defgeneric (fun-name lambda-list &body options)
  (crate:check-can-defun-symbol name)
  `(progn
     (cl:defgeneric ,name ,lambda-list ,@options)))
