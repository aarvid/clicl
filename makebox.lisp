;;;; makebox.lisp
;; Copyright © 2014-2015 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see LICENSE.txt (BSD-2 License)

(in-package #:clicl)







(defun make-sandbox (name)
  (let* ((box (make-instance 'sandbox :name name))
         (crate  (sandbox-crate box))
         (crate:*crate* crate))
    (crate:with-crate-locks-disabled (crate :cl)
      (process-symbol-treatment box *shadow-cl-symbols*))
    (let ((clicl-pkg (crate:make-package :clicl)))
      (crate:shadow-external-symbol crate 'clicl:quit)
      (crate:shadow-external-symbol crate 'clicl:load-system)
      (crate:shadow-external-symbol crate 'clicl:list-systems)
      (setf (crate:package-symbol-locked-p clicl-pkg) t)
      (setf (crate:package-define-locked-p clicl-pkg) t))
    box))

#|(eval-when (:load-toplevel :execute)
  (unless *sandbox*
    (setf *sandbox* (make-sandbox "TEST-BOX"))))|#



(defun box-read-string (box string)
   (crate:with-crate (crate (sandbox-crate box))
     (with-input-from-string (s string)
       (clicl-reader:read s))))
