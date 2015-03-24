;;;; conditions.lisp
;; Copyright © 2014-2015 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see LICENSE.txt (BSD-2 License)


(in-package #:clicl)


(define-condition sandbox-error (error) ()
  (:report "Unknown sandbox-error"))


(define-condition shadow-package-exists (sandbox-error)
    ((name :initarg :name :reader pkg-name))
  (:report (lambda (c s)
             (format s "The package ~a already has been shadowed"
                     (pkg-name c)))))

(define-condition  repl-read-done (sandbox-error) nil)

(define-condition  repl-quit-signal (sandbox-error) nil)


(define-condition  illegal-sharp-reader-macro (sandbox-error)
  ((schar :initarg :schar :reader sharp-char))
  (:report (lambda (c s)
             (format s "illegal sharp macro character: ~S"
                     (sharp-char c)))))

(define-condition  box-locked-symbol (sandbox-error)
  ((name :initarg :name :reader boxed-out-name))
  (:report (lambda (c s)
             (format s "The symbol ~a has been sandboxed"
                     (boxed-out-name c)))))

(define-condition  circular-lisp-form (sandbox-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (print "The lisp form is circular" s))))

(define-condition box-form-dimension-error (sandbox-error)
  ((type :initarg :name :reader form-type)
   (max-dimension :initarg :max-dimension :reader max-dimension)
   (actual-dimension :initarg :actual-dimension :reader actual-dimension))
  (:report (lambda (c s)
             (format s "~a form has size ~a. Sandbox does not support forms of total size greater than ~a"
                     (form-type c) (actual-dimension c) (max-dimension c)))))

(define-condition box-unsupported-type (sandbox-error)
  ((type :initarg :name :reader unsupported-type))
  (:report (lambda (c s)
             (format s "Sandbox does not support type ~a in lisp forms"
                     (unsupported-type c)))))

(define-condition boxed-out-function (sandbox-error)
  ((name :initarg :name :reader boxed-out-name))
  (:report (lambda (c s)
             (format s "The function ~a has been sandboxed"
                     (boxed-out-name c)))))

(define-condition boxed-out-macro (sandbox-error)
  ((name :initarg :name :reader boxed-out-name))
  (:report (lambda (c s)
             (format s "The macro ~a has been sandboxed"
                     (boxed-out-name c)))))

(define-condition boxed-out-setf (sandbox-error)
  ((name :initarg :name :reader boxed-out-name))
  (:report (lambda (c s)
             (format s "The setf ~a has been sandboxed"
                     (boxed-out-name c)))))

(define-condition sandbox-unknown-system (sandbox-error)
  ((name :initarg :name :reader system-name))
  (:report (lambda (c s)
             (format s "The system ~a is unknown to the sandbox"
                     (system-name c)))))

