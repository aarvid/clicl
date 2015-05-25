(in-package :cl-user)

(asdf:defsystem #:clicl-feebs
  :serial t
  :description "Planet of the Feebs in the Clicl Sandbox"
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "BSD-2"
  :depends-on (#:alexandria
               #:clicl
               #:feebs)
  :components ((:file "package")
               (:file "cleebs")
               ))
