;; Copyright © 2014 Andy Peterson <andy.arvid@gmail.com>
;; This work is free. You can redistribute it and/or modify it under the
;; terms of the Do What The Fuck You Want To Public License, Version 2,
;; as published by Sam Hocevar. See the COPYING file for more details.

;; This program is free software. It comes without any warranty, to
;; the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the Do What The Fuck You Want
;; To Public License, Version 2, as published by Sam Hocevar. See
;; http://www.wtfpl.net/ for more details.

;;;; clicl.asd

(asdf:defsystem #:clicl
  :serial t
  :description "Common Lisp(s) in Common Lisp - A sandboxed interface to the underlying lisp"
  :author "Andy Peterson <andy.arvid@gmail.com>"
  :license "WTFPL"
  :depends-on (#:alexandria
               #:named-readtables
               #:trivial-timeout)
  :components ((:file "package")
               (:file "conditions")
               (:file "sandbox")
               #+sbcl
               (:file "shadow-sbcl")
               (:file "shadow-cl")
               (:file "shadow-alex")
               (:file "makebox")))

