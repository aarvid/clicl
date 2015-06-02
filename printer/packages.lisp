(cl:in-package :cl-user)

(cl:defpackage #:clicl-printer
  (:use #:cl)
  (:shadow . #1=(#:write
                 #:print))
  (:export #:print-with-position
           . #1#))



