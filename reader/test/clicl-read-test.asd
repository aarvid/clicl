(cl:in-package #:common-lisp-user)

(asdf:defsystem :clicl-read-test
  :depends-on (:lisp-unit :clicl-read)
  :components
  ((:file "packages" :depends-on ())
   (:file "test" :depends-on (packages))))
