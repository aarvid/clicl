
(asdf:defsystem "clicl-read"
  :description "Reader for clicl based on sicl-read of SICL by Robert Strandh"
  :author "Robert Strandh, Andy Peterson <andy.arvid@gmail.com>"
  :license "Public Domain"
  :depends-on ("crate")
  :components ((:file "packages" :depends-on ())
               (:file "float" :depends-on ("packages"))
               (:file "read" :depends-on ("packages" "float"))))
