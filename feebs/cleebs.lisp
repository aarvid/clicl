(in-package :clicl-feebs)

(defparameter *shadow-feebs-symbols*
  '(
    (feebs:feeb-name :shadow)
    ))

(clicl::def-clicl-system :feebs '(:feebs) *shadow-feebs-symbols*)


