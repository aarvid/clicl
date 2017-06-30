
;; Copyright © 2014-2015 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see LICENSE.txt (BSD-2 License)

;;;; clicl.asd

(asdf:defsystem "clicl"
  :serial t
  :description "Common Lisp(s) in Common Lisp - A sandboxed interface to the underlying lisp"
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "BSD-2"
  :depends-on ("alexandria"
               "cl-unicode"
               "named-readtables"
               "trivial-timeout"
               "crate"
               "clicl-reader-simple"
               "clicl-printer")
  :components ((:file "package")
               (:file "conditions")
               (:file "sandbox")
               (:file "redefines")
               (:file "shadow-cl")
               (:file "shadow-alex")
               (:file "makebox")
               (:file "systems")))
