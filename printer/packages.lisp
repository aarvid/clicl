(cl:in-package :cl-user)

(cl:defpackage #:clicl-printer
  (:use #:cl)
  (:shadow . #1=(#:write
                 #:print
                 #:princ
                 #:prin1
                 #:pprint
                 #:format))
  (:export #:foo.bar
           . #1#))



