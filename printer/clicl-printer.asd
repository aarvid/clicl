(in-package #:common-lisp-user)

(asdf:defsystem :clicl-printer
  :description "Printer for clicl/crate based on code from Corman Lisp"
  :author "Roger G Corman, Andy Peterson <andy.arvid@gmail.com>"
  :license "MIT"
  :depends-on (#:crate #:closer-mop)
  :components ((:file "packages" :depends-on ())
               (:file "write" :depends-on ("packages"))
               (:file "format" :depends-on ("packages" "write"))))
