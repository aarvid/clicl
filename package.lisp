;;;; package.lisp
;; Copyright © 2014 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see License.txt


(in-package :cl-user)

(defpackage #:clicl
  (:use #:cl #:alexandria)
  (:export #:quit
           #:make-sandbox
           #:repl-command-line
           #:repl-string
           #:repl-stream
           #:sandbox
           #:sandbox-name
           #:sandbox-package
           #:sandbox-symbols
           ))



