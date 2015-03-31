;;;; makebox.lisp
;; Copyright © 2014-2015 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see LICENSE.txt (BSD-2 License)

(in-package #:clicl)





(defun make-alexandria-package (sandbox)
  (if-let ((pkg (cl:find-package :alexandria.0.dev)))
    (crate:with-crate (crate (sandbox-crate sandbox))
      (let ((shdw (or (crate:find-package (package-name pkg))
                      (crate:promote-inferior-package crate pkg))))
        (process-symbol-treatment sandbox *shadow-alexandria-symbols*)
        shdw))
    (error "Alexandria is not available")))


(defun load-system* (sandbox system)
  (case system
    (:alexandria (make-alexandria-package sandbox))
    (otherwise (error 'sandbox-unknown-system :name system))))

(defun load-system (system)
  (load-system* *sandbox* system))




(defun make-sandbox (name)
  (let* ((box (make-instance 'sandbox :name name))
         (crate  (sandbox-crate box))
         (crate:*crate* crate))
    (process-symbol-treatment box *shadow-cl-symbols*)
    (crate:make-package :clicl)
    (crate:shadow-external-symbol crate 'clicl:quit)
    (crate:shadow-external-symbol crate 'clicl::load-system)
    box))

#|(eval-when (:load-toplevel :execute)
  (unless *sandbox*
    (setf *sandbox* (make-sandbox "TEST-BOX"))))|#



(defun box-read-string (box string)
   (crate:with-crate (crate (sandbox-crate box))
     (with-input-from-string (s string)
       (clicl-read:read s))))
