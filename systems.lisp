;;;; systems.lisp
;; Copyright © 2014-2015 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see LICENSE.txt (BSD-2 License)

(in-package #:clicl)


(defparameter *clicl-systems* nil)

(defclass clicl-system ()
  ((name :accessor system-name :initarg :name)
   (packages :accessor system-packages :initarg :packages)
   (symbols :accessor system-symbols :initarg :symbols)
   ))


(defmacro def-clicl-system (name packages symbols)
  `(pushnew (cons ,name
                  (make-instance 'clicl-system
                                 :name ,name :packages ,packages
                                 :symbols ,symbols))
            *clicl-systems*))

(def-clicl-system :alexandria '(:alexandria.0.dev) *shadow-alexandria-symbols*)

(defun list-systems ()
  (mapcar #'car *clicl-systems*))

(defun get-system (name)
  (cdr (assoc name *clicl-systems*)))

(defun load-system* (sandbox system-name)
  (if-let ((system (get-system system-name)))
    (let ((pkgs (mapcar #'cl:find-package (system-packages system))))
      (crate:with-crate (crate (sandbox-crate sandbox))
        (let ((shdws (mapcar (lambda (pkg)
                               (or (crate:find-package (package-name pkg))
                                   (crate:promote-inferior-package crate pkg)))
                             pkgs)))
          (process-symbol-treatment sandbox (system-symbols system))
          (mapc (lambda (shdw)
                  (setf (crate:package-symbol-locked-p shdw) t)
                  (setf (crate:package-define-locked-p shdw) t))
                shdws)
          (system-name system))))
    (error 'sandbox-unknown-system :name system)))

(defun load-system (system)
  (load-system* *sandbox* system))
