;;;; shadow-sbcl.lisp
;; Copyright © 2014 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see License.txt


(in-package #:clicl)



;;;#+sbcl
(defparameter *shadow-sbcl-symbols*
  nil
  #|'((sb-impl::backq-list :shadow)
    (sb-impl::backq-list* :shadow)
    (sb-impl::backq-append :shadow)
    (sb-impl::backq-append :shadow)
    (sb-impl::backq-nconc :shadow)
    (sb-impl::backq-vector :shadow))|#)
