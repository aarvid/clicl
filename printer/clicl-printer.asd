(in-package #:common-lisp-user)

(asdf:defsystem :clicl-printer
  :description "Printerer for clicl based on Corman Lisp"
  :author "Roger G Corman, Andy Peterson <andy.arvid@gmail.com>"
  :license "MIT"
  :depends-on (#:crate)
  :components ((:file "packages" :depends-on ())
               (:file "write" :depends-on ("packages"))))
