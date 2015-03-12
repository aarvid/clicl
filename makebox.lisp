;;;; makebox.lisp
;; Copyright © 2014 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see License.txt




(in-package #:clicl)



(defun make-shadow-package (sandbox package)
  (let* ((package (if (packagep package)
                      package
                      (find-package package)))
         (name (package-name package))
         (shadow (make-package (gensym name) :use nil)))
    (when (get-shadow-package sandbox package)
      (error 'shadow-package-exists :name name))
    (setf (gethash name (shadow-packages sandbox))
          shadow)
    (setf (gethash (package-name shadow) (sandbox-packages sandbox))
          shadow)))


(defun user-make-package (sandbox package-name &key nicknames use)
  (declare (ignore nicknames use))
  (let ((pkg (or (find-package package-name)
                 (make-package package-name))))
    (when (get-shadow-package sandbox pkg)
      (error 'shadow-package-exists :name package-name))
    (make-shadow-package sandbox pkg)))



(defun make-sbcl-package (sandbox)
  (let ((pkg (make-shadow-package sandbox :sb-impl )))
    (process-symbol-treatment sandbox *shadow-sbcl-symbols*)
    pkg))

(defun make-cl-package (sandbox)
  (let ((pkg (make-shadow-package sandbox :common-lisp )))
    (process-symbol-treatment sandbox *shadow-cl-symbols*)
    pkg))

(defun make-alexandria-package (sandbox)
  (if-let ((pkg (find-package :alexandria.0.dev)))
    (let ((shdw (or (get-shadow-package sandbox pkg)
                    (make-shadow-package sandbox pkg))))
      (process-symbol-treatment sandbox *shadow-alexandria-symbols*)
      shdw)
    (error "Alexandria is not available")))


(defun load-system* (sandbox system)
  (case system
    (:alexandria (make-alexandria-package sandbox))
    (otherwise (error 'sandbox-unknown-system :name system))))

(defun load-system (system)
  (load-system* *sandbox* system))



(defun make-clicl-package (sandbox)
  (let ((pkg (make-shadow-package sandbox :clicl )))
    (box-shadow-symbol sandbox 'clicl:quit)
    (box-shadow-symbol sandbox 'clicl::load-system)
    pkg))

(defun make-sandbox (name)
  (let* ((box (make-instance 'sandbox :name name))
         (cl-user (make-shadow-package box :cl-user)))
    (make-sbcl-package box)
    (cl:use-package (list (make-clicl-package box)
                          (make-cl-package box))
                    cl-user)
    (setf (sandbox-package box)
          cl-user)
    box))

(eval-when (:load-toplevel :execute)
  (unless *sandbox*
    (setf *sandbox* (make-sandbox "TEST-BOX"))))

(defun new-sandbox (name)
  (let* ((box (make-instance 'sandbox :name name))
         (crate  (sandbox-crate box))
         (crate:*packs* (crate:crate-packages crate)))
    (setf (crate:common-lisp-package crate)
          (crate:make-package :common-lisp :nicknames '(:cl)))
    (setf (crate:keyword-package crate)
          (crate:make-package :keyword))
    (crate:make-package :clicl :use '(:cl))
    (crate:shadow-external-symbol crate 'clicl:quit)
    (crate:shadow-external-symbol crate 'clicl::load-system)
    (setf (crate:common-lisp-user-package crate)
          (crate:make-package :common-lisp-user :use '(:cl :clicl) :nicknames '(:cl-user)))

    box))

