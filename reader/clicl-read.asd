(in-package #:common-lisp-user)

(asdf:defsystem :clicl-read
  :depends-on (#:crate)
  :components ((:file "packages" :depends-on ())
               (:file "float" :depends-on ("packages"))
               (:file "read" :depends-on ("packages" "float"))))
