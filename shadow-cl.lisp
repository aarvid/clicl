;;;; shadow-cl.lisp
;; Copyright © 2014-2015 Andrew Arvid Peterson <andy.arvid@gmail.com>
;; see LICENSE.txt (BSD-2 License)

(in-package #:clicl)


#|
 ((3 . "Evaluation and Compilation")
  (&allow-other-keys &aux &body &environment &key &optional &rest &whole
   *macroexpand-hook* compilation-speed compile compiler-macro-function
   constantp debug declaim declaration declare define-compiler-macro
   define-symbol-macro defmacro dynamic-extent eval eval-when ftype ignorable
   ignore inline lambda load-time-value locally macro-function macroexpand
   macroexpand-1 notinline optimize proclaim quote safety space special
   special-operator-p speed symbol-macrolet the type))
|#





#|(macroexpand '(defmacro foo (name ll &body body)
               (when (symbol-locked-p sandbox name)
                 (error 'box-locked-symbol :name name))
               `(cl:defmacro ,name ,ll ,@body)))|#

(defparameter *shadow-3-evaluation-and-compilation*
  '((cl:&allow-other-keys :shadow)
    (cl:&aux :shadow)
    (cl:&body :shadow)
    (cl:&environment :shadow)
    (cl:&key :shadow)
    (cl:&optional :shadow)
    (cl:&rest :shadow)
    (cl:&whole :shadow)
    (cl:*macroexpand-hook* :ignore)
    (cl:compilation-speed :ignore)
    (cl:compile :box-out)
    (cl:compiler-macro-function :box-out)
    (cl:constantp :shadow)
    (cl:debug :ignore)
    (cl:declaim :box-out)
    (cl:declaration :ignore)
    (cl:declare :box-out)
    (cl:define-compiler-macro :box-out)
    (cl:define-symbol-macro :box-out)
    (cl:defmacro :box-out)
    (cl:dynamic-extent :shadow)
    (cl:eval :box-out)
    (cl:eval-when :box-out)
    (cl:ftype :shadow)
    (cl:ignorable :shadow)
    (cl:shadow :ignore)
    (cl:inline :ignore)
    (cl:lambda :shadow)
    (cl:load-time-value :box-out)
    (cl:locally :ignore)
    (cl:macro-function :box-out)
    (cl:macroexpand :shadow)
    (cl:macroexpand-1 :shadow)
    (cl:notinline :ignore)
    (cl:optimize :ignore)
    (cl:proclaim :box-out)
    (cl:quote :shadow)
    (cl:safety  :ignore)
    (cl:space :ignore)
    (cl:special :ignore)
    (cl:special-operator-p :shadow)
    (cl:speed :ignore)
    (cl:symbol-macrolet :box-out)
    (cl:the :box-out)
    (cl:type :ignore)))


#|
 ((4 . "Types and Classes")
  (boolean built-in-class class coerce compiled-function deftype
   generic-function method method-combination
   satisfies simple-type-error standard-class
   standard-generic-function standard-method standard-object structure-class
   structure-object subtypep type-error type-error-datum
   type-error-expected-type type-of typep ))
|#

(defparameter *shadow-4-types-and-classes*
  '((cl:boolean :shadow)
    (cl:built-in-class :shadow)
    (cl:class :shadow)
    (cl:coerce :shadow)
    (cl:compiled-function :shadow)
    (cl:deftype :box-out)
    (cl:generic-function :shadow)
    (cl:method :shadow)
    (cl:method-combination :shadow)
    (cl:satisfies :shadow)
    (cl:simple-type-error :ignore)
    (cl:standard-class :shadow)
    (cl:standard-generic-function :shadow)
    (cl:standard-method :shadow)
    (cl:standard-object :shadow)
    (cl:structure-class :shadow)
    (cl:structure-object :shadow)
    (cl:subtypep :shadow)
    (cl:type-error :ignore)
    (cl:type-error-datum :shadow)
    (cl:type-error-expected-type :shadow)
    (cl:type-of :shadow)
    (cl:typep :shadow)))

#|
 ((5 . "Data and Control Flow")
  (and apply block call-arguments-limit case catch ccase compiled-function-p
       complement cond constantly control-error ctypecase defconstant
       define-modify-macro define-setf-expander defparameter defsetf defun
       defvar destructuring-bind ecase eq eql equal equalp etypecase every
       fboundp fdefinition flet fmakunbound funcall function
       function-lambda-expression functionp get-setf-expansion go identity if
       labels lambda-list-keywords lambda-parameters-limit let let* macrolet
       multiple-value-bind multiple-value-call multiple-value-list
       multiple-value-prog1 multiple-value-setq multiple-values-limit nil not
       notany notevery nth-value or otherwise prog prog* prog1 prog2 progn
       program-error progv psetf psetq return return-from rotatef setf setq
       shiftf some t tagbody throw typecase undefined-function unless
       unwind-protect values values-list when))
|#

(defparameter *shadow-5-data-and-control-flow*
  '((cl:and :shadow)
    (cl:apply :shadow)
    (cl:block :shadow)
    (cl:call-arguments-limit :shadow)
    (cl:case :shadow)
    (cl:catch :box-out)
    (cl:ccase :shadow)
    (cl:compiled-function-p :shadow)
    (cl:complement :shadow)
    (cl:cond :shadow)
    (cl:constantly :shadow)
    (cl:control-error :ignore)
    (cl:ctypecase :shadow)
    (cl:defconstant :shadow)
    (cl:define-modify-macro :box-out)
    (cl:define-setf-expander :box-out)
    (cl:defparameter :shadow)
    (cl:defsetf :box-out)
    (cl:defun  :shadow :clicl-redefines)
    (cl:defvar :shadow)
    (cl:destructuring-bind :shadow)
    (cl:ecase :shadow)
    (cl:eq :shadow)
    (cl:eql :shadow)
    (cl:equal :shadow)
    (cl:equalp :shadow)
    (cl:etypecase :shadow)
    (cl:every :shadow)
    (cl:fboundp :shadow)
    (cl:fdefinition :box-out)
    (cl:flet :shadow)
    (cl:fmakunbound :box-out)
    (cl:funcall :shadow)
    (cl:function :shadow)
    (cl:function-lambda-expression :box-out)
    (cl:functionp :shadow)
    (cl:get-setf-expansion :box-out)
    (cl:go :shadow)
    (cl:identity :shadow)
    (cl:if :shadow)
    (cl:labels :shadow)
    (cl:lambda-list-keywords :shadow)
    (cl:lambda-parameters-limit :shadow)
    (cl:let :shadow)
    (cl:let* :shadow)
    (cl:macrolet :shadow)
    (cl:multiple-value-bind :shadow)
    (cl:multiple-value-call :shadow)
    (cl:multiple-value-list :shadow)
    (cl:multiple-value-prog1 :shadow)
    (cl:multiple-value-setq :shadow)
    (cl:multiple-values-limit :shadow)
    (cl:nil :shadow)
    (cl:not :shadow)
    (cl:notany :shadow)
    (cl:notevery :shadow)
    (cl:nth-value :shadow)
    (cl:or :shadow)
    (cl:otherwise :shadow)
    (cl:prog :shadow)
    (cl:prog* :shadow)
    (cl:prog1 :shadow)
    (cl:prog2 :shadow)
    (cl:progn :shadow)
    (cl:program-error :ignore)
    (cl:progv :shadow)
    (cl:psetf :shadow)
    (cl:psetq :shadow)
    (cl:return :shadow)
    (cl:return-from :shadow)
    (cl:rotatef :shadow)
    (cl:setf :shadow)
    (cl:setq :shadow)
    (cl:shiftf :shadow)
    (cl:some :shadow)
    (cl:t :shadow)
    (cl:tagbody :shadow)
    (cl:throw :box-out)
    (cl:typecase :shadow)
    (cl:undefined-function :ignore #| condition |#) 
    (cl:unless :shadow)
    (cl:unwind-protect :box-out)
    (cl:values :shadow)
    (cl:values-list :shadow)
    (cl:when :shadow)))

#|
 ((6 . "Iteration")
  (do do* dolist dotimes loop loop-finish))
|#

(defparameter *shadow-6-iteration*
  '((cl:do :shadow)
    (cl:do* :shadow)
    (cl:dolist :shadow)
    (cl:dotimes :shadow)
    (cl:loop :box-out)
    (cl:loop-finish :box-out)))
#|
 ((7 . "Objects")
  (add-method allocate-instance call-method call-next-method change-class
              class-name class-of compute-applicable-methods defclass
              defgeneric define-method-combination defmethod
              ensure-generic-function find-class find-method function-keywords
              initialize-instance make-instance make-instances-obsolete
              make-load-form make-load-form-saving-slots make-method
              method-qualifiers next-method-p no-applicable-method
              no-next-method reinitialize-instance remove-method
              shared-initialize slot-boundp slot-exists-p slot-makunbound
              slot-missing slot-unbound slot-value standard unbound-slot
              unbound-slot-instance update-instance-for-different-class
              update-instance-for-redefined-class with-accessors with-slots))
|#

(defparameter *shadow-7-objects*
  '((cl:add-method :box-out)
    (cl:allocate-instance :box-out)
    (cl:call-method :box-out)
    (cl:call-next-method :box-out)
    (cl:change-class :box-out)
    (cl:class-name :box-out)
    (cl:class-of :shadow)
    (cl:compute-applicable-methods :box-out)
    (cl:defclass :box-out)
    (cl:defgeneric :box-out)
    (cl:define-method-combination :box-out)
    (cl:defmethod :box-out)
    (cl:ensure-generic-function :box-out)
    (cl:find-class :box-out)
    (cl:find-method :box-out)
    (cl:function-keywords :box-out)
    (cl:initialize-instance :box-out)
    (cl:make-instance :box-out)
    (cl:make-instances-obsolete :box-out)
    (cl:make-load-form :box-out)
    (cl:make-load-form-saving-slots :box-out)
    (cl:make-method :box-out)
    (cl:method-qualifiers :box-out)
    (cl:next-method-p :box-out)
    (cl:no-applicable-method :box-out)
    (cl:no-next-method :box-out)
    (cl:reinitialize-instance :box-out)
    (cl:remove-method :box-out)
    (cl:shared-initialize :box-out)
    (cl:slot-boundp :box-out)
    (cl:slot-exists-p :box-out)
    (cl:slot-makunbound :box-out)
    (cl:slot-missing :box-out)
    (cl:slot-unbound :box-out)
    (cl:slot-value :box-out)
    (cl:standard :ignore)
    (cl:unbound-slot :ignore)
    (cl:unbound-slot-instance :box-out)
    (cl:update-instance-for-different-class :box-out)
    (cl:update-instance-for-redefined-class :box-out)
    (cl:with-accessors :box-out)
    (cl:with-slots :box-out)))


#|
 ((8 . "Structures")
  (copy-structure defstruct))
|#

(defparameter *shadow-8-structures*
  '((cl:copy-structure :shadow)
    (cl:defstruct :shadow)))

#|
 ((9 . "Conditions")
  (*break-on-signals* *debugger-hook* abort assert break cell-error
   cell-error-name cerror check-type compute-restarts condition continue
   define-condition error find-restart handler-bind handler-case ignore-errors
   invalid-method-error invoke-debugger invoke-restart
   invoke-restart-interactively make-condition method-combination-error
   muffle-warning parse-error restart restart-bind restart-case restart-name
   serious-condition signal simple-condition simple-condition-format-arguments
   simple-condition-format-control simple-error simple-warning
   storage-condition store-value style-warning use-value warn warning
   with-condition-restarts with-simple-restart))
|#


(defparameter *shadow-9-conditions*
  '((cl:*break-on-signals* :ignore)
    (cl:*debugger-hook* :ignore)
    (cl:abort :box-out)
    (cl:assert :box-out)
    (cl:break :box-out)
    (cl:cell-error :ignore)
    (cl:cell-error-name :box-out)
    (cl:cerror :box-out)
    (cl:check-type :box-out)
    (cl:compute-restarts :box-out)
    (cl:condition :ignore)
    (cl:continue :box-out)
    (cl:define-condition :box-out)
    (cl:error :box-out)
    (cl:find-restart :box-out)
    (cl:handler-bind :box-out)
    (cl:handler-case :box-out)
    (cl:ignore-errors :box-out)
    (cl:invalid-method-error :ignore)
    (cl:invoke-debugger :box-out)
    (cl:invoke-restart :box-out)
    (cl:invoke-restart-interactively :box-out)
    (cl:make-condition :box-out)
    (cl:method-combination-error :box-out)
    (cl:muffle-warning :box-out)
    (cl:parse-error :ignore)
    (cl:restart :shadow)
    (cl:restart-bind :box-out)
    (cl:restart-case :box-out)
    (cl:restart-name :box-out)
    (cl:serious-condition :ignore)
    (cl:signal :box-out)
    (cl:simple-condition :ignore)
    (cl:simple-condition-format-arguments :box-out)
    (cl:simple-condition-format-control :box-out)
    (cl:simple-error :ignore)
    (cl:simple-warning :ignore)
    (cl:storage-condition :ignore)
    (cl:store-value :box-out)
    (cl:style-warning :ignore)
    (cl:use-value :box-out)
    (cl:warn :box-out)
    (cl:warning :ignore)
    (cl:with-condition-restarts :box-out)
    (cl:with-simple-restart :box-out)))


#|
 ((10 . "Symbols")
  (*gensym-counter* boundp copy-symbol gensym gentemp get keyword keywordp
   make-symbol makunbound remprop set symbol symbol-function symbol-name
   symbol-package symbol-plist symbol-value symbolp unbound-variable))
|#

(defparameter *shadow-10-symbols*
  '((cl:*gensym-counter* :ignore)
    (cl:boundp :shadow)
    (cl:copy-symbol :box-out)
    (cl:gensym :box-out)
    (cl:gentemp :box-out)
    (cl:get :box-out)
    (cl:keyword :shadow)
    (cl:keywordp :shadow)
    (cl:make-symbol :shadow :crate)
    (cl:makunbound :box-out)
    (cl:remprop :box-out)
    (cl:set :box-out)
    (cl:symbol :shadow)
    (cl:symbol-function :box-out)
    (cl:symbol-name :shadow)
    (cl:symbol-package :shadow :crate)
    (cl:symbol-plist :box-out)
    (cl:symbol-value :box-out)
    (cl:symbolp :shadow)
    (cl:unbound-variable :ignore)))
#|
 ((11 . "Packages")
  (*package* defpackage delete-package do-all-symbols do-external-symbols
   do-symbols export find-all-symbols find-package find-symbol import
   in-package intern list-all-packages make-package package package-error
   package-error-package package-name package-nicknames
   package-shadowing-symbols package-use-list package-used-by-list packagep
   rename-package shadow shadowing-import unexport unintern unuse-package
   use-package with-package-iterator))
|#


(defparameter *shadow-11-packages*
  '((cl:*package* :echo)
    (cl:defpackage :shadow :crate)
    (cl:delete-package :shadow :crate)
    (cl:do-all-symbols :shadow :crate)
    (cl:do-external-symbols :shadow :crate)
    (cl:do-symbols :shadow :crate)
    (cl:export :shadow :crate)
    (cl:find-all-symbols :shadow :crate)
    (cl:find-package :shadow :crate)
    (cl:find-symbol :shadow :crate)
    (cl:import :shadow :crate)
    (cl:in-package :shadow :crate)
    (cl:intern :shadow :crate)
    (cl:list-all-packages :shadow :crate)
    (cl:make-package :shadow :crate)
    (cl:package :shadow :crate)
    (cl:package-error :shadow :crate)
    (cl:package-error-package :shadow :crate)
    (cl:package-name :shadow :crate)
    (cl:package-nicknames :shadow :crate)
    (cl:package-shadowing-symbols :shadow :crate)
    (cl:package-use-list :shadow :crate)
    (cl:package-used-by-list :shadow :crate)
    (cl:packagep :shadow :crate)
    (cl:rename-package :shadow :crate)
    (cl:shadow :shadow :crate)
    (cl:shadowing-import :shadow :crate)
    (cl:unexport :shadow :crate)
    (cl:unintern :shadow :crate)
    (cl:unuse-package :shadow :crate)
    (cl:use-package :shadow :crate)
    (cl:with-package-iterator :shadow :crate)))

#|
 ((12 . "Numbers")
  (* *random-state* + - / /= 1+ 1- < <= = > >= abs acos acosh arithmetic-error
     arithmetic-error-operands arithmetic-error-operation ash asin asinh atan
     atanh bignum boole boole-1 boole-2 boole-and boole-andc1 boole-andc2
     boole-c1 boole-c2 boole-clr boole-eqv boole-ior boole-nand boole-nor
     boole-orc1 boole-orc2 boole-set boole-xor byte byte-position byte-size
     ceiling cis complex complexp conjugate cos cosh decf decode-float
     denominator deposit-field division-by-zero double-float
     double-float-epsilon double-float-negative-epsilon dpb evenp exp expt
     fceiling ffloor fixnum float float-digits float-precision float-radix
     float-sign floating-point-inexact floating-point-invalid-operation
     floating-point-overflow floating-point-underflow floatp floor fround
     ftruncate gcd imagpart incf integer integer-decode-float integer-length
     integerp isqrt lcm ldb ldb-test least-negative-double-float
     least-negative-long-float least-negative-normalized-double-float
     least-negative-normalized-long-float least-negative-normalized-short-float
     least-negative-normalized-single-float least-negative-short-float
     least-negative-single-float least-positive-double-float
     least-positive-long-float least-positive-normalized-double-float
     least-positive-normalized-long-float least-positive-normalized-short-float
     least-positive-normalized-single-float least-positive-short-float
     least-positive-single-float log logand logandc1 logandc2 logbitp logcount
     logeqv logior lognand lognor lognot logorc1 logorc2 logtest logxor
     long-float long-float-epsilon long-float-negative-epsilon
     make-random-state mask-field max min minusp mod most-negative-double-float
     most-negative-fixnum most-negative-long-float most-negative-short-float
     most-negative-single-float most-positive-double-float most-positive-fixnum
     most-positive-long-float most-positive-short-float
     most-positive-single-float number numberp numerator oddp parse-integer
     phase pi plusp random random-state random-state-p ratio rational
     rationalize rationalp real realp realpart rem round scale-float
     short-float short-float-epsilon short-float-negative-epsilon signed-byte
     signum sin single-float single-float-epsilon single-float-negative-epsilon
     sinh sqrt tan tanh truncate unsigned-byte upgraded-complex-part-type
     zerop))
|#

(defun redefine-global-var (sandbox old-symbol new-symbol)
  (declare (ignore sandbox old-symbol))
  (eval `(defparameter ,new-symbol nil)))


(defun redefine-dual-global-var-function (sandbox old-symbol new-symbol)
  (redefine-global-var sandbox old-symbol new-symbol)
  (setf (symbol-function new-symbol)
        (lambda (&rest args)
          (apply (symbol-function old-symbol) args))))


(defparameter *shadow-12-numbers*
  '((cl:* :redefine redefine-dual-global-var-function)
    (cl:*random-state* :ignore)
    (cl:+ :redefine redefine-dual-global-var-function)
    (cl:- :redefine redefine-dual-global-var-function)
    (cl:/ :redefine redefine-dual-global-var-function)
    (cl:/= :shadow)
    (cl:1+ :shadow)
    (cl:1- :shadow)
    (cl:< :shadow)
    (cl:<= :shadow)
    (cl:= :shadow)
    (cl:> :shadow)
    (cl:>= :shadow)
    (cl:abs :shadow)
    (cl:acos :shadow)
    (cl:acosh :shadow)
    (cl:arithmetic-error :ignore)
    (cl:arithmetic-error-operands :box-out)
    (cl:arithmetic-error-operation :box-out)
    (cl:ash :shadow)
    (cl:asin :shadow)
    (cl:asinh :shadow)
    (cl:atan :shadow)
    (cl:atanh :shadow)
    (cl:bignum :shadow)
    (cl:boole :shadow)
    (cl:boole-1 :shadow)
    (cl:boole-2 :shadow)
    (cl:boole-and :shadow)
    (cl:boole-andc1 :shadow)
    (cl:boole-andc2 :shadow)
    (cl:boole-c1 :shadow)
    (cl:boole-c2 :shadow)
    (cl:boole-clr :shadow)
    (cl:boole-eqv :shadow)
    (cl:boole-ior :shadow)
    (cl:boole-nand :shadow)
    (cl:boole-nor :shadow)
    (cl:boole-orc1 :shadow)
    (cl:boole-orc2 :shadow)
    (cl:boole-set :shadow)
    (cl:boole-xor :shadow)
    (cl:byte :shadow)
    (cl:byte-position :shadow)
    (cl:byte-size :shadow)
    (cl:ceiling :shadow)
    (cl:cis :shadow)
    (cl:complex :shadow)
    (cl:complexp :shadow)
    (cl:conjugate :shadow)
    (cl:cos :shadow)
    (cl:cosh :shadow)
    (cl:decf :shadow)
    (cl:decode-float :shadow)
    (cl:denominator :shadow)
    (cl:deposit-field :shadow)
    (cl:division-by-zero :ignore)
    (cl:double-float :shadow)
    (cl:double-float-epsilon :shadow)
    (cl:double-float-negative-epsilon :shadow)
    (cl:dpb :shadow)
    (cl:evenp :shadow)
    (cl:exp :shadow)
    (cl:expt :shadow)
    (cl:fceiling :shadow)
    (cl:ffloor :shadow)
    (cl:fixnum :shadow)
    (cl:float :shadow)
    (cl:float-digits :shadow)
    (cl:float-precision :shadow)
    (cl:float-radix :shadow)
    (cl:float-sign :shadow)
    (cl:floating-point-inexact :ignore)
    (cl:floating-point-invalid-operation :ignore)
    (cl:floating-point-overflow :ignore)
    (cl:floating-point-underflow :ignore)
    (cl:floatp :shadow)
    (cl:floor :shadow)
    (cl:fround :shadow)
    (cl:ftruncate :shadow)
    (cl:gcd :shadow)
    (cl:imagpart :shadow)
    (cl:incf :shadow)
    (cl:integer :shadow)
    (cl:integer-decode-float :shadow)
    (cl:integer-length :shadow)
    (cl:integerp :shadow)
    (cl:isqrt :shadow)
    (cl:lcm :shadow)
    (cl:ldb :shadow)
    (cl:ldb-test :shadow)
    (cl:least-negative-double-float :shadow)
    (cl:least-negative-long-float :shadow)
    (cl:least-negative-normalized-double-float :shadow)
    (cl:least-negative-normalized-long-float :shadow)
    (cl:least-negative-normalized-short-float :shadow)
    (cl:least-negative-normalized-single-float :shadow)
    (cl:least-negative-short-float :shadow)
    (cl:least-negative-single-float :shadow)
    (cl:least-positive-double-float :shadow)
    (cl:least-positive-long-float :shadow)
    (cl:least-positive-normalized-double-float :shadow)
    (cl:least-positive-normalized-long-float :shadow)
    (cl:least-positive-normalized-short-float :shadow)
    (cl:least-positive-normalized-single-float :shadow)
    (cl:least-positive-short-float :shadow)
    (cl:least-positive-single-float :shadow)
    (cl:log :shadow)
    (cl:logand :shadow)
    (cl:logandc1 :shadow)
    (cl:logandc2 :shadow)
    (cl:logbitp :shadow)
    (cl:logcount :shadow)
    (cl:logeqv :shadow)
    (cl:logior :shadow)
    (cl:lognand :shadow)
    (cl:lognor :shadow)
    (cl:lognot :shadow)
    (cl:logorc1 :shadow)
    (cl:logorc2 :shadow)
    (cl:logtest :shadow)
    (cl:logxor :shadow)
    (cl:long-float :shadow)
    (cl:long-float-epsilon :shadow)
    (cl:long-float-negative-epsilon :shadow)
    (cl:make-random-state :shadow)
    (cl:mask-field :shadow)
    (cl:max :shadow)
    (cl:min :shadow)
    (cl:minusp :shadow)
    (cl:mod :shadow)
    (cl:most-negative-double-float :shadow)
    (cl:most-negative-fixnum :shadow)
    (cl:most-negative-long-float :shadow)
    (cl:most-negative-short-float :shadow)
    (cl:most-negative-single-float :shadow)
    (cl:most-positive-double-float :shadow)
    (cl:most-positive-fixnum :shadow)
    (cl:most-positive-long-float :shadow)
    (cl:most-positive-short-float :shadow)
    (cl:most-positive-single-float :shadow)
    (cl:number :shadow)
    (cl:numberp :shadow)
    (cl:numerator :shadow)
    (cl:oddp :shadow)
    (cl:parse-integer :shadow)
    (cl:phase :shadow)
    (cl:pi :shadow)
    (cl:plusp :shadow)
    (cl:random :shadow)
    (cl:random-state :shadow)
    (cl:random-state-p :shadow)
    (cl:ratio :shadow)
    (cl:rational :shadow)
    (cl:rationalize :shadow)
    (cl:rationalp :shadow)
    (cl:real :shadow)
    (cl:realp :shadow)
    (cl:realpart :shadow)
    (cl:rem :shadow)
    (cl:round :shadow)
    (cl:scale-float :shadow)
    (cl:short-float :shadow)
    (cl:short-float-epsilon :shadow)
    (cl:short-float-negative-epsilon :shadow)
    (cl:signed-byte :shadow)
    (cl:signum :shadow)
    (cl:sin :shadow)
    (cl:single-float :shadow)
    (cl:single-float-epsilon :shadow)
    (cl:single-float-negative-epsilon :shadow)
    (cl:sinh :shadow)
    (cl:sqrt :shadow)
    (cl:tan :shadow)
    (cl:tanh :shadow)
    (cl:truncate :shadow)
    (cl:unsigned-byte :shadow)
    (cl:upgraded-complex-part-type :shadow)
    (cl:zerop :shadow)))

#|
 ((13 . "Characters")
  (alpha-char-p alphanumericp base-char both-case-p char-code char-code-limit
                char-downcase char-equal char-greaterp char-int char-lessp
                char-name char-not-equal char-not-greaterp char-not-lessp
                char-upcase char/= char< char<= char= char> char>= character
                characterp code-char digit-char digit-char-p extended-char
                graphic-char-p lower-case-p name-char standard-char
                standard-char-p upper-case-p))
|#

(defparameter *shadow-13-characters*
  '((cl:alpha-char-p :shadow)
    (cl:alphanumericp :shadow)
    (cl:base-char :shadow)
    (cl:both-case-p :shadow)
    (cl:char-code :shadow)
    (cl:char-code-limit :shadow)
    (cl:char-downcase :shadow)
    (cl:char-equal :shadow)
    (cl:char-greaterp :shadow)
    (cl:char-int :shadow)
    (cl:char-lessp :shadow)
    (cl:char-name :shadow)
    (cl:char-not-equal :shadow)
    (cl:char-not-greaterp :shadow)
    (cl:char-not-lessp :shadow)
    (cl:char-upcase :shadow)
    (cl:char/= :shadow)
    (cl:char< :shadow)
    (cl:char<= :shadow)
    (cl:char= :shadow)
    (cl:char> :shadow)
    (cl:char>= :shadow)
    (cl:character :shadow)
    (cl:characterp :shadow)
    (cl:code-char :shadow)
    (cl:digit-char :shadow)
    (cl:digit-char-p :shadow)
    (cl:extended-char :shadow)
    (cl:graphic-char-p :shadow)
    (cl:lower-case-p :shadow)
    (cl:name-char :shadow)
    (cl:standard-char :shadow)
    (cl:standard-char-p :shadow)
    (cl:upper-case-p :shadow)))

#|
 ((14 . "Conses")
  (acons adjoin append assoc assoc-if assoc-if-not atom butlast caaaar caaadr
         caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr
         cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr
         cddar cdddar cddddr cdddr cddr cdr cons consp copy-alist copy-list
         copy-tree eighth endp fifth first fourth get-properties getf
         intersection last ldiff list list* list-length listp make-list mapc
         mapcan mapcar mapcon mapl maplist member member-if member-if-not
         nbutlast nconc nintersection ninth nreconc nset-difference
         nset-exclusive-or nsublis nsubst nsubst-if nsubst-if-not nth nthcdr
         null nunion pairlis pop push pushnew rassoc rassoc-if rassoc-if-not
         remf rest revappend rplaca rplacd second set-difference
         set-exclusive-or seventh sixth sublis subsetp subst subst-if
         subst-if-not tailp tenth third tree-equal union))
|#


(defparameter *shadow-14-conses*
  '((cl:acons :shadow)
    (cl:adjoin :shadow)
    (cl:append :shadow)
    (cl:assoc :shadow)
    (cl:assoc-if :shadow)
    (cl:assoc-if-not :shadow)
    (cl:atom :shadow)
    (cl:butlast :shadow)
    (cl:caaaar :shadow)
    (cl:caaadr :shadow)
    (cl:caaar :shadow)
    (cl:caadar :shadow)
    (cl:caaddr :shadow)
    (cl:caadr :shadow)
    (cl:caar :shadow)
    (cl:cadaar :shadow)
    (cl:cadadr :shadow)
    (cl:cadar :shadow)
    (cl:caddar :shadow)
    (cl:cadddr :shadow)
    (cl:caddr :shadow)
    (cl:cadr :shadow)
    (cl:car :shadow)
    (cl:cdaaar :shadow)
    (cl:cdaadr :shadow)
    (cl:cdaar :shadow)
    (cl:cdadar :shadow)
    (cl:cdaddr :shadow)
    (cl:cdadr :shadow)
    (cl:cdar :shadow)
    (cl:cddaar :shadow)
    (cl:cddadr :shadow)
    (cl:cddar :shadow)
    (cl:cdddar :shadow)
    (cl:cddddr :shadow)
    (cl:cdddr :shadow)
    (cl:cddr :shadow)
    (cl:cdr :shadow)
    (cl:cons :shadow)
    (cl:consp :shadow)
    (cl:copy-alist :shadow)
    (cl:copy-list :shadow)
    (cl:copy-tree :shadow)
    (cl:eighth :shadow)
    (cl:endp :shadow)
    (cl:fifth :shadow)
    (cl:first :shadow)
    (cl:fourth :shadow)
    (cl:get-properties :shadow)
    (cl:getf :shadow)
    (cl:intersection :shadow)
    (cl:last :shadow)
    (cl:ldiff :shadow)
    (cl:list :shadow)
    (cl:list* :shadow)
    (cl:list-length :shadow)
    (cl:listp :shadow)
    (cl:make-list :shadow)
    (cl:mapc :shadow)
    (cl:mapcan :shadow)
    (cl:mapcar :shadow)
    (cl:mapcon :shadow)
    (cl:mapl :shadow)
    (cl:maplist :shadow)
    (cl:member :shadow)
    (cl:member-if :shadow)
    (cl:member-if-not :shadow)
    (cl:nbutlast :shadow)
    (cl:nconc :shadow)
    (cl:nintersection :shadow)
    (cl:ninth :shadow)
    (cl:nreconc :shadow)
    (cl:nset-difference :shadow)
    (cl:nset-exclusive-or :shadow)
    (cl:nsublis :shadow)
    (cl:nsubst :shadow)
    (cl:nsubst-if :shadow)
    (cl:nsubst-if-not :shadow)
    (cl:nth :shadow)
    (cl:nthcdr :shadow)
    (cl:null :shadow)
    (cl:nunion :shadow)
    (cl:pairlis :shadow)
    (cl:pop :shadow)
    (cl:push :shadow)
    (cl:pushnew :shadow)
    (cl:rassoc :shadow)
    (cl:rassoc-if :shadow)
    (cl:rassoc-if-not :shadow)
    (cl:remf :shadow)
    (cl:rest :shadow)
    (cl:revappend :shadow)
    (cl:rplaca :shadow)
    (cl:rplacd :shadow)
    (cl:second :shadow)
    (cl:set-difference :shadow)
    (cl:set-exclusive-or :shadow)
    (cl:seventh :shadow)
    (cl:sixth :shadow)
    (cl:sublis :shadow)
    (cl:subsetp :shadow)
    (cl:subst :shadow)
    (cl:subst-if :shadow)
    (cl:subst-if-not :shadow)
    (cl:tailp :shadow)
    (cl:tenth :shadow)
    (cl:third :shadow)
    (cl:tree-equal :shadow)
    (cl:union :shadow)))


#|
 ((15 . "Arrays")
  (adjust-array adjustable-array-p aref array array-dimension
                array-dimension-limit array-dimensions array-displacement
                array-element-type array-has-fill-pointer-p array-in-bounds-p
                array-rank array-rank-limit array-row-major-index
                array-total-size array-total-size-limit arrayp bit bit-and
                bit-andc1 bit-andc2 bit-eqv bit-ior bit-nand bit-nor bit-not
                bit-orc1 bit-orc2 bit-vector bit-vector-p bit-xor fill-pointer
                make-array row-major-aref sbit simple-array simple-bit-vector
                simple-bit-vector-p simple-vector simple-vector-p svref
                upgraded-array-element-type vector vector-pop vector-push
                vector-push-extend vectorp))
|#

(defparameter *shadow-15-arrays*
  '((cl:adjust-array :shadow)
    (cl:adjustable-array-p :shadow)
    (cl:aref :shadow)
    (cl:array :shadow)
    (cl:array-dimension :shadow)
    (cl:array-dimension-limit :shadow)
    (cl:array-dimensions :shadow)
    (cl:array-displacement :shadow)
    (cl:array-element-type :shadow)
    (cl:array-has-fill-pointer-p :shadow)
    (cl:array-in-bounds-p :shadow)
    (cl:array-rank :shadow)
    (cl:array-rank-limit :shadow)
    (cl:array-row-major-index :shadow)
    (cl:array-total-size :shadow)
    (cl:array-total-size-limit :shadow)
    (cl:arrayp :shadow)
    (cl:bit :shadow)
    (cl:bit-and :shadow)
    (cl:bit-andc1 :shadow)
    (cl:bit-andc2 :shadow)
    (cl:bit-eqv :shadow)
    (cl:bit-ior :shadow)
    (cl:bit-nand :shadow)
    (cl:bit-nor :shadow)
    (cl:bit-not :shadow)
    (cl:bit-orc1 :shadow)
    (cl:bit-orc2 :shadow)
    (cl:bit-vector :shadow)
    (cl:bit-vector-p :shadow)
    (cl:bit-xor :shadow)
    (cl:fill-pointer :shadow)
    (cl:make-array :shadow)
    (cl:row-major-aref :shadow)
    (cl:sbit :shadow)
    (cl:simple-array :shadow)
    (cl:simple-bit-vector :shadow)
    (cl:simple-bit-vector-p :shadow)
    (cl:simple-vector :shadow)
    (cl:simple-vector-p :shadow)
    (cl:svref :shadow)
    (cl:upgraded-array-element-type :shadow)
    (cl:vector :shadow)
    (cl:vector-pop :shadow)
    (cl:vector-push :shadow)
    (cl:vector-push-extend :shadow)
    (cl:vectorp :shadow)))

#|
 ((16 . "Strings")
  (base-string char make-string nstring-capitalize nstring-downcase
   nstring-upcase schar simple-base-string simple-string simple-string-p string
   string-capitalize string-downcase string-equal string-greaterp
   string-left-trim string-lessp string-not-equal string-not-greaterp
   string-not-lessp string-right-trim string-trim string-upcase string/=
   string< string<= string= string> string>= stringp))
|#

(defparameter *shadow-16-strings*
  '((cl:base-string :shadow)
    (cl:char :shadow)
    (cl:make-string :shadow)
    (cl:nstring-capitalize :shadow)
    (cl:nstring-downcase :shadow)
    (cl:nstring-upcase :shadow)
    (cl:schar :shadow)
    (cl:simple-base-string :shadow)
    (cl:simple-string :shadow)
    (cl:simple-string-p :shadow)
    (cl:string :shadow)
    (cl:string-capitalize :shadow)
    (cl:string-downcase :shadow)
    (cl:string-equal :shadow)
    (cl:string-greaterp :shadow)
    (cl:string-left-trim :shadow)
    (cl:string-lessp :shadow)
    (cl:string-not-equal :shadow)
    (cl:string-not-greaterp :shadow)
    (cl:string-not-lessp :shadow)
    (cl:string-right-trim :shadow)
    (cl:string-trim :shadow)
    (cl:string-upcase :shadow)
    (cl:string/= :shadow)
    (cl:string< :shadow)
    (cl:string<= :shadow)
    (cl:string= :shadow)
    (cl:string> :shadow)
    (cl:string>= :shadow)
    (cl:stringp :shadow)))


#|
 ((17 . "Sequences")
  (concatenate copy-seq count count-if count-if-not delete delete-duplicates
               delete-if delete-if-not elt fill find find-if find-if-not length
               make-sequence map map-into merge mismatch nreverse nsubstitute
               nsubstitute-if nsubstitute-if-not position position-if
               position-if-not reduce remove remove-duplicates remove-if
               remove-if-not replace reverse search sequence sort stable-sort
               subseq substitute substitute-if substitute-if-not))
|#

(defparameter *shadow-17-sequences*
  '((cl:concatenate :shadow)
    (cl:copy-seq :shadow)
    (cl:count :shadow)
    (cl:count-if :shadow)
    (cl:count-if-not :shadow)
    (cl:delete :shadow)
    (cl:delete-duplicates :shadow)
    (cl:delete-if :shadow)
    (cl:delete-if-not :shadow)
    (cl:elt :shadow)
    (cl:fill :shadow)
    (cl:find :shadow)
    (cl:find-if :shadow)
    (cl:find-if-not :shadow)
    (cl:length :shadow)
    (cl:make-sequence :shadow)
    (cl:map :shadow)
    (cl:map-into :shadow)
    (cl:merge :shadow)
    (cl:mismatch :shadow)
    (cl:nreverse :shadow)
    (cl:nsubstitute :shadow)
    (cl:nsubstitute-if :shadow)
    (cl:nsubstitute-if-not :shadow)
    (cl:position :shadow)
    (cl:position-if :shadow)
    (cl:position-if-not :shadow)
    (cl:reduce :shadow)
    (cl:remove :shadow)
    (cl:remove-duplicates :shadow)
    (cl:remove-if :shadow)
    (cl:remove-if-not :shadow)
    (cl:replace :shadow)
    (cl:reverse :shadow)
    (cl:search :shadow)
    (cl:sequence :shadow)
    (cl:sort :shadow)
    (cl:stable-sort :shadow)
    (cl:subseq :shadow)
    (cl:substitute :shadow)
    (cl:substitute-if :shadow)
    (cl:substitute-if-not :shadow)))


#|
 ((18 . "Hash Tables")
  (clrhash gethash hash-table hash-table-count hash-table-p
           hash-table-rehash-size hash-table-rehash-threshold hash-table-size
           hash-table-test make-hash-table maphash remhash sxhash
           with-hash-table-iterator))
|#

(defparameter *shadow-18-hash-tables*
  '((cl:clrhash :shadow)
    (cl:gethash :shadow)
    (cl:hash-table :shadow)
    (cl:hash-table-count :shadow)
    (cl:hash-table-p :shadow)
    (cl:hash-table-rehash-size :shadow)
    (cl:hash-table-rehash-threshold :shadow)
    (cl:hash-table-size :shadow)
    (cl:hash-table-test :shadow)
    (cl:make-hash-table :shadow)
    (cl:maphash :shadow)
    (cl:remhash :shadow)
    (cl:sxhash :shadow)
    (cl:with-hash-table-iterator :shadow)))

#|
 ((19 . "Filenames")
  (*default-pathname-defaults* directory-namestring enough-namestring
   file-namestring host-namestring load-logical-pathname-translations
   logical-pathname logical-pathname-translations make-pathname merge-pathnames
   namestring parse-namestring pathname pathname-device pathname-directory
   pathname-host pathname-match-p pathname-name pathname-type pathname-version
   pathnamep translate-logical-pathname translate-pathname wild-pathname-p))
|#

(defparameter *shadow-19-filenames*
  '((cl:*default-pathname-defaults* :ignore)
    (cl:directory-namestring :shadow)
    (cl:enough-namestring :box-out)
    (cl:file-namestring :shadow)
    (cl:host-namestring :box-out)
    (cl:load-logical-pathname-translations :box-out)
    (cl:logical-pathname :box-out)
    (cl:logical-pathname-translations :box-out)
    (cl:make-pathname :shadow)
    (cl:merge-pathnames :shadow)
    (cl:namestring :shadow)
    (cl:parse-namestring :shadow)
    (cl:pathname :shadow)
    (cl:pathname-device :box-out)
    (cl:pathname-directory :shadow)
    (cl:pathname-host :box-out)
    (cl:pathname-match-p :shadow)
    (cl:pathname-name :shadow)
    (cl:pathname-type :shadow)
    (cl:pathname-version :shadow)
    (cl:pathnamep :shadow)
    (cl:translate-logical-pathname :box-out)
    (cl:translate-pathname :shadow)
    (cl:wild-pathname-p :shadow)))


#|
 ((20 . "Files")
  (delete-file directory ensure-directories-exist file-author file-error
               file-error-pathname file-write-date probe-file rename-file
               truename))
|#

(defparameter *shadow-20-files*
  '((cl:delete-file :box-out)
    (cl:directory :box-out)
    (cl:ensure-directories-exist :box-out)
    (cl:file-author :box-out)
    (cl:file-error :ignore)
    (cl:file-error-pathname :box-out)
    (cl:file-write-date :box-out)
    (cl:probe-file :box-out)
    (cl:rename-file :box-out)
    (cl:truename :box-out)))

#|
 ((21 . "Streams")
  (*debug-io* *error-output* *query-io* *standard-input* *standard-output*
   *terminal-io* *trace-output* broadcast-stream broadcast-stream-streams
   clear-input clear-output close concatenated-stream
   concatenated-stream-streams echo-stream echo-stream-input-stream
   echo-stream-output-stream end-of-file file-length file-position file-stream
   file-string-length finish-output force-output fresh-line
   get-output-stream-string input-stream-p interactive-stream-p listen
   make-broadcast-stream make-concatenated-stream make-echo-stream
   make-string-input-stream make-string-output-stream make-synonym-stream
   make-two-way-stream open open-stream-p output-stream-p peek-char read-byte
   read-char read-char-no-hang read-line read-sequence stream
   stream-element-type stream-error stream-error-stream stream-external-format
   streamp string-stream synonym-stream synonym-stream-symbol terpri
   two-way-stream two-way-stream-input-stream two-way-stream-output-stream
   unread-char with-input-from-string with-open-file with-open-stream
   with-output-to-string write-byte write-char write-line write-sequence
   write-string y-or-n-p yes-or-no-p))
|#

(defparameter *shadow-21-streams*
  '((cl:*debug-io* :ignore)
    (cl:*error-output* :ignore)
    (cl:*query-io* :ignore)
    (cl:*standard-input* :ignore)
    (cl:*standard-output* :ignore)
    (cl:*terminal-io* :ignore)
    (cl:*trace-output* :ignore)
    (cl:broadcast-stream :ignore)
    (cl:broadcast-stream-streams :box-out)
    (cl:clear-input :shadow)
    (cl:clear-output :shadow)
    (cl:close :shadow)
    (cl:concatenated-stream :shadow)
    (cl:concatenated-stream-streams :box-out)
    (cl:echo-stream :shadow)
    (cl:echo-stream-input-stream :ignore)
    (cl:echo-stream-output-stream :ignore)
    (cl:end-of-file :ignore)
    (cl:file-length :box-out)
    (cl:file-position :shadow)
    (cl:file-stream :shadow)
    (cl:file-string-length :box-out)
    (cl:finish-output :shadow)
    (cl:force-output :shadow)
    (cl:fresh-line :shadow)
    (cl:get-output-stream-string :ignore)
    (cl:input-stream-p :shadow)
    (cl:interactive-stream-p :shadow)
    (cl:listen :shadow)
    (cl:make-broadcast-stream :box-out)
    (cl:make-concatenated-stream :box-out)
    (cl:make-echo-stream :box-out)
    (cl:make-string-input-stream :box-out)
    (cl:make-string-output-stream :box-out)
    (cl:make-synonym-stream :box-out)
    (cl:make-two-way-stream :box-out)
    (cl:open :box-out)
    (cl:open-stream-p :shadow)
    (cl:output-stream-p :shadow)
    (cl:peek-char :shadow)
    (cl:read-byte :box-out)
    (cl:read-char :shadow)
    (cl:read-char-no-hang :shadow)
    (cl:read-line :shadow)
    (cl:read-sequence :shadow)
    (cl:stream :shadow)
    (cl:stream-element-type :shadow)
    (cl:stream-error :ignore)
    (cl:stream-error-stream :box-out)
    (cl:stream-external-format :box-out)
    (cl:streamp :shadow)
    (cl:string-stream :shadow)
    (cl:synonym-stream :shadow)
    (cl:synonym-stream-symbol :box-out)
    (cl:terpri :shadow)
    (cl:two-way-stream :shadow)
    (cl:two-way-stream-input-stream :box-out)
    (cl:two-way-stream-output-stream :box-out)
    (cl:unread-char :shadow)
    (cl:with-input-from-string :shadow)
    (cl:with-open-file :box-out)
    (cl:with-open-stream :box-out)
    (cl:with-output-to-string :shadow)
    (cl:write-byte :shadow)
    (cl:write-char :shadow)
    (cl:write-line :shadow)
    (cl:write-sequence :shadow)
    (cl:write-string :shadow)
    (cl:y-or-n-p :box-out)
    (cl:yes-or-no-p :box-out)))


#|
 ((22 . "Printer")
  (*print-array* *print-base* *print-case* *print-circle* *print-escape*
   *print-gensym* *print-length* *print-level* *print-lines*
   *print-miser-width* *print-pprint-dispatch* *print-pretty* *print-radix*
   *print-readably* *print-right-margin* copy-pprint-dispatch format formatter
   pprint pprint-dispatch pprint-exit-if-list-exhausted pprint-fill
   pprint-indent pprint-linear pprint-logical-block pprint-newline pprint-pop
   pprint-tab pprint-tabular prin1 prin1-to-string princ princ-to-string print
   print-not-readable print-not-readable-object print-object
   print-unreadable-object set-pprint-dispatch write write-to-string))
|#

(defparameter *shadow-22-printer*
  '((cl:*print-array* :ignore)
    (cl:*print-base* :ignore)
    (cl:*print-case* :ignore)
    (cl:*print-circle* :ignore)
    (cl:*print-escape* :ignore)
    (cl:*print-gensym* :ignore)
    (cl:*print-length* :ignore)
    (cl:*print-level* :ignore)
    (cl:*print-lines* :ignore)
    (cl:*print-miser-width* :ignore)
    (cl:*print-pprint-dispatch* :ignore)
    (cl:*print-pretty* :ignore)
    (cl:*print-radix* :ignore)
    (cl:*print-readably* :ignore)
    (cl:*print-right-margin* :ignore)
    (cl:copy-pprint-dispatch :box-out)
    (cl:format :box-out)
    (cl:formatter :box-out)
    (cl:pprint :shadow)
    (cl:pprint-dispatch :box-out)
    (cl:pprint-exit-if-list-exhausted :box-out)
    (cl:pprint-fill :box-out)
    (cl:pprint-indent :box-out)
    (cl:pprint-linear :box-out)
    (cl:pprint-logical-block :box-out)
    (cl:pprint-newline :box-out)
    (cl:pprint-pop :box-out)
    (cl:pprint-tab :box-out)
    (cl:pprint-tabular :box-out)
    (cl:prin1 :shadow)
    (cl:prin1-to-string :shadow)
    (cl:princ :shadow)
    (cl:princ-to-string :shadow)
    (cl:print :shadow)
    (cl:print-not-readable :ignore)
    (cl:print-not-readable-object :box-out)
    (cl:print-object :box-out)
    (cl:print-unreadable-object :box-out)
    (cl:set-pprint-dispatch :box-out)
    (cl:write :shadow)
    (cl:write-to-string :shadow)))

#|
 ((23 . "Reader")
  (*read-base* *read-default-float-format* *read-eval* *read-suppress*
   *readtable* copy-readtable get-dispatch-macro-character get-macro-character
   make-dispatch-macro-character read read-delimited-list read-from-string
   read-preserving-whitespace reader-error readtable readtable-case readtablep
   set-dispatch-macro-character set-macro-character set-syntax-from-char
   with-standard-io-syntax))
|#



(defparameter *shadow-23-reader*
  '((cl:*read-base* :ignore)
    (cl:*read-default-float-format* :ignore)
    (cl:*read-eval* :ignore)
    (cl:*read-suppress* :ignore)
    (cl:*readtable* :ignore)
    (cl:copy-readtable :box-out)
    (cl:get-dispatch-macro-character :box-out)
    (cl:get-macro-character :box-out)
    (cl:make-dispatch-macro-character :box-out)
    (cl:read :box-out) 
    (cl:read-delimited-list :box-out)
    (cl:read-from-string :box-out) 
    (cl:read-preserving-whitespace :box-out)
    (cl:reader-error :box-out)
    (cl:readtable :box-out)
    (cl:readtable-case :box-out)
    (cl:readtablep :box-out)
    (cl:set-dispatch-macro-character :box-out)
    (cl:set-macro-character :box-out)
    (cl:set-syntax-from-char :box-out)
    (cl:with-standard-io-syntax :box-out)))

#|
 ((24 . "System Construction")
  (*compile-file-pathname* *compile-file-truename* *compile-print*
   *compile-verbose* *features* *load-pathname* *load-print* *load-truename*
   *load-verbose* *modules* compile-file compile-file-pathname load provide
   require with-compilation-unit))
|#

(defparameter *shadow-24-system-construction*
  '((cl:*compile-file-pathname* :ignore)
    (cl:*compile-file-truename* :ignore)
    (cl:*compile-print* :ignore)
    (cl:*compile-verbose* :ignore)
    (cl:*features* :ignore)
    (cl:*load-pathname* :ignore)
    (cl:*load-print* :ignore)
    (cl:*load-truename* :ignore)
    (cl:*load-verbose* :ignore)
    (cl:*modules* :ignore)
    (cl:compile-file :box-out)
    (cl:compile-file-pathname :box-out)
    (cl:load :box-out)
    (cl:provide :box-out)
    (cl:require :box-out)
    (cl:with-compilation-unit :box-out)))


#|
 ((25 . "Environment")
  (** *** ++ +++ // /// apropos apropos-list compiler-macro
   decode-universal-time describe describe-object disassemble documentation
   dribble ed encode-universal-time get-decoded-time get-internal-real-time
   get-internal-run-time get-universal-time inspect
   internal-time-units-per-second lisp-implementation-type
   lisp-implementation-version long-site-name machine-instance machine-type
   machine-version room short-site-name sleep software-type
   software-version step structure time trace untrace user-homedir-pathname
   variable))
|#


(defparameter *shadow-25-environment
  '((cl:** :redefine redefine-global-var)
    (cl:*** :redefine redefine-global-var)
    (cl:++ :redefine redefine-global-var)
    (cl:+++ :redefine redefine-global-var)
    (cl:// :redefine redefine-global-var)
    (cl:/// :redefine redefine-global-var)
    (cl:apropos :box-out)
    (cl:apropos-list :box-out)
    (cl:compiler-macro :ignore)
    (cl:decode-universal-time :shadow)
    (cl:describe :box-out)
    (cl:describe-object :box-out)
    (cl:disassemble :box-out)
    (cl:documentation :box-out)
    (cl:dribble :box-out)
    (cl:ed :box-out)
    (cl:encode-universal-time :shadow)
    (cl:get-decoded-time :shadow)
    (cl:get-internal-real-time :box-out)
    (cl:get-internal-run-time :box-out)
    (cl:get-universal-time :shadow)
    (cl:inspect :box-out)
    (cl:internal-time-units-per-second :ignore)
    (cl:lisp-implementation-type :shadow)
    (cl:lisp-implementation-version :shadow)
    (cl:long-site-name :box-out)
    (cl:machine-instance :box-out)
    (cl:machine-type :box-out)
    (cl:machine-version :box-out)
    (cl:room :box-out)
    (cl:short-site-name :box-out)
    (cl:sleep :shadow)
    (cl:software-type :box-out)
    (cl:software-version :box-out)
    (cl:step :box-out)
    (cl:structure :ignore)
    (cl:time :box-out)
    (cl:trace :box-out)
    (cl:untrace :box-out)
    (cl:user-homedir-pathname :box-out)
    (cl:variable :ignore)))
 
(defparameter *shadow-cl-symbols*
  (concatenate 'list
               *shadow-3-evaluation-and-compilation*
               *shadow-4-types-and-classes*
               *shadow-5-data-and-control-flow*
               *shadow-6-iteration*
               *shadow-7-objects*
               *shadow-8-structures*
               *shadow-9-conditions*
               *shadow-10-symbols*
               *shadow-11-packages*
               *shadow-12-numbers*
               *shadow-13-characters*
               *shadow-14-conses*
               *shadow-15-arrays*
               *shadow-16-strings*
               *shadow-17-sequences*
               *shadow-18-hash-tables*
               *shadow-19-filenames*
               *shadow-20-files*
               *shadow-21-streams*
               *shadow-22-printer*
               *shadow-23-reader*
               *shadow-24-system-construction*
               *shadow-25-environment))
