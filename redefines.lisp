(cl:in-package :cl-user)

(cl:defpackage #:clicl-redefines
  (:use #:cl #:alexandria)
  (:shadow #:defun
           #:defmacro
           #:defgeneric
           #:defmethod
           #:defclass)
  (:export #:defun
           #:defmacro
           #:defgeneric
           #:defmethod
           #:defclass
           ))

;;; this package/file contains macros and functions for the sandbox
;;; that are redefined.

(cl:defmacro clicl-redefines:defun (name lambda-list cl:&body body)
  (crate:check-can-define-symbol name)
  `(progn
     (cl:defun ,name ,lambda-list ,@body)))

(cl:defmacro clicl-redefines:defmacro (name lambda-list cl:&body body)
  (crate:check-can-define-symbol name)
  `(progn
     (cl:defmacro ,name ,lambda-list ,@body)))

;; defgeneric, defmethod

(cl:defmacro clicl-redefines:defgeneric (name lambda-list cl:&body options)
  (crate:check-can-define-symbol name)
  `(progn
     (cl:defgeneric ,name ,lambda-list ,@options)))

(cl:defmacro clicl-redefines:defmethod (name cl:&rest args)
  (crate:check-can-define-symbol name)
  `(progn
     (cl:defmethod ,name ,@args)))

(cl:defmacro clicl-redefines:defclass (name direct-superclasses
                                       direct-slots cl:&rest options)
  (crate:check-can-define-symbol name)
  `(progn
     (cl:defclass ,name ,direct-superclasses ,direct-slots ,@options)))

