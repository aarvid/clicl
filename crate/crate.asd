
(asdf:defsystem #:crate
  :serial t
  :description "Crate: a container for packages. Based on zpackage by Xach and modifications by PJB"
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "BSD"
  :depends-on (#:alexandria)
  :components ((:file "package" :depends-on ())
               (:file "crate" :depends-on ("package"))))


