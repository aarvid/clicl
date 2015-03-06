
(asdf:defsystem #:crate
  :serial t
  :description "Crate: a container for packages. Based on zpackage by Xach and modifications by PJB"
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "BSD"
  :components ((:file "package-pac" :depends-on ())
               (:file "package-mac" :depends-on ("package-pac"))
               (:file "package-fun" :depends-on ("package-pac"
                                                 "package-mac"))
               (:file "crate" :depends-on ("package-pac"
                                           "package-mac"
                                           "package-fun"))
               #|(:file "package-def" :depends-on ("package-pac"
                                                 "package-mac"
                                                 "package-fun"))|#))


