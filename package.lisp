;;;; package.lisp
;; Copyright © 2014-2015 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see LICENSE.txt (BSD-2 License)



(cl:in-package :cl-user)

(cl:defpackage #:clicl
  (:use #:cl #:alexandria)
  (:export #:quit
           #:make-sandbox
           #:repl-command-line ;;?
           #:repl
           #:repl-string
           #:repl-stream
           #:sandbox
           #:sandbox-name
           #:sandbox-package
           #:sandbox-symbols
           #:load-system
           #:list-systems
           ))





