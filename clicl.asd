;; Copyright © 2014 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see License.txt (MIT License)

;;;; clicl.asd

(asdf:defsystem #:clicl
  :serial t
  :description "Common Lisp(s) in Common Lisp - A sandboxed interface to the underlying lisp"
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-unicode
               #:named-readtables
               #:trivial-timeout)
  :components ((:file "package")
               (:file "conditions")
               ;(:file "sreader")
               (:file "sandbox")
               #+sbcl
               (:file "shadow-sbcl")
               (:file "shadow-cl")
               (:file "shadow-alex")
               (:file "makebox")))

