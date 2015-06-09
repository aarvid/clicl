(in-package #:common-lisp-user)

(asdf:defsystem :clicl-printer
  :description "Printer for clicl based on Corman Lisp"
  :author "Roger G Corman, Andy Peterson <andy.arvid@gmail.com>"
  :license "MIT"
  :depends-on (#:crate)
  :components ((:file "packages" :depends-on ())
               (:file "cwrite" :depends-on ("packages"))))
