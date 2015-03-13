;;;; makebox.lisp
;; Copyright © 2014 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see License.txt




(in-package #:clicl)





(defun make-alexandria-package (sandbox)
  (if-let ((pkg (cl:find-package :alexandria.0.dev)))
    (crate:with-crate (crate (sandbox-crate sandbox))
      (let ((shdw (or (crate:find-package (package-name pkg))
                      (crate:copy-genuine-package crate pkg))))
        (process-symbol-treatment sandbox *shadow-alexandria-symbols*)
        shdw))
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
         (crate  (sandbox-crate box))
         (crate:*crate* crate))
    (setf (crate:common-lisp-package crate)
          (crate:make-package :common-lisp :nicknames '(:cl)))
    (setf (crate:keyword-package crate)
          (crate:make-package :keyword))
    (crate:make-package :clicl :use '(:cl))
    (setf (crate:common-lisp-user-package crate)
          (crate:make-package :common-lisp-user :use '(:cl :clicl)
                                                :nicknames '(:cl-user)))
    (process-symbol-treatment box *shadow-cl-symbols*)
    (crate:shadow-external-symbol crate 'clicl:quit)
    (crate:shadow-external-symbol crate 'clicl::load-system)
    (crate:crate-set-current-package crate
                                     (crate:common-lisp-user-package crate))
    box))

#|(eval-when (:load-toplevel :execute)
  (unless *sandbox*
    (setf *sandbox* (make-sandbox "TEST-BOX"))))|#



(defun box-read-string (box string)
   (crate:with-crate (crate (sandbox-crate box))
     (with-input-from-string (s string)
       (clicl-read:read s))))
