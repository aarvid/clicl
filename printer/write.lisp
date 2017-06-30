;;;;    -------------------------------
;;;;    Copyright (c) Corman Technologies Inc.
;;;;    See LICENSE.txt for license information.
;;;;    -------------------------------
;;;;
;;;;    File:           write.lisp
;;;;    Contents:       Corman Lisp startup code to build the
;;;;                            system.
;;;;    History:        09/01/96 RGC Created.
;;;;                    11/30/99 RGC Modified floating point printing
;;;;                                 to be ANSI-compliant.
;;;;                    04/16/01 RGC Writing arrays now handles fill-pointers correctly.
;;;;                    09/29/01 RGC Incorporated fix to WRITE-SYMBOL from JP Massar.
;;;;                    12/06/01 FAA Updated structure printer for wider slot specifiers.
;;;;                    09/22/03 RGC BIT-VECTORs with fill pointers now print correctly.
;;;;                    29/06/15 AAP modifications for clicl/crate 
;;;;

(in-package :clicl-printer)

(defconstant left-paren #\( )
(defconstant right-paren #\) )

(defvar *printer-eq-forms* nil) ;; not exported
(defvar *printer-eq-forms-index* 0) ;; not exported
(defvar *current-print-level* 0) ;; not exported


(defun structurep (object)
  (typep object 'structure-object))

(defun get-printer-eq-form (form)
  (let ((f (gethash form *printer-eq-forms* 0)))
    (or (listp f)(> f 1))))
;;;
;;; Add support for structures to this
;;;
(defun search-for-circularities (object)
  ;; This currently checks arrays (of general type), lists, and structures
  ;;
  (unless
      (or (consp object)
          (structurep object)
          (typep object '(array t)))
    (return-from search-for-circularities))
  (let ((n (gethash object *printer-eq-forms* 0)))
    (setf (gethash object *printer-eq-forms*) (+ n 1))
    body...)(if (= n 0)
    (cond ((consp object)
           (search-for-circularities (car object))
           (search-for-circularities (cdr object)))
          ((structurep object)
           (dolist (slot (mapcar #'c2mop:slot-definition-name
                                 (c2mop:class-slots object)))
             (search-for-circularities (slot-value object slot))))
          (t (let ((size (apply '* (array-dimensions object))))
               (dotimes (i size)
                 (search-for-circularities (row-major-aref object i))))))))


(defun %output-char (char stream)
  (cl:write char :stream stream :escape nil))

(defun %output-chars (chars stream start stop)
  (cl:write (subseq chars start stop) :stream stream :escape nil))

(defun write-list (object)
  (let ((os *standard-output*)
        list)
    (if *print-pretty*
        (progn
          (output-pretty-list object os)
          (return-from write-list)))

    ;; check for (quote x) forms and output as 'x
    (if (and (eq (car object) 'quote)
             (consp (cdr object)))
        (progn
          (%output-char #\' os)
          (write-lisp-object (cadr object))
          (return-from write-list)))

    ;; check for (function x) forms and output as #'x
    (if (and (eq (car object) 'function)
             (consp (cdr object)))
        (progn
          (%output-char #\# os)
          (%output-char #\' os)
          (write-lisp-object (cadr object))
          (return-from write-list)))

    (incf *current-print-level*) ;; increment the print level

    (%output-char left-paren os)
    (block print-loop
      (setq list object)
      (do* ((count 0 (+ count 1)))
           ((not (consp list)))
        (when (> count 0)
          (%output-char #\space os)
          (if (and *print-circle* (get-printer-eq-form list))
              (return-from print-loop)))
        (if (and (not *print-readably*) *print-length* (>= *print-length* 0)
                 (>= count *print-length*))
            (progn
              (%output-chars "..." os 0 3)
              (%output-char right-paren os)
              (decf *current-print-level*)
              (return-from write-list))) ;; decrement the print level
        (write-lisp-object (car list))
        (setq list (cdr list))))

    (if list
        (progn
          (%output-chars " . " os 0 3)
          (write-lisp-object list)))
    (%output-char right-paren os)
    (decf *current-print-level*)))



(defun special-char-p (char)
  (if (member char '(#\| #\# #\( #\) #\\ #\: #\;)) t nil))

(defun whitespace-char-p (char)
  (if (member char '(#\Space #\Tab)) t nil))

(defun external-symbol-p (sym package)
  (eq (cadr (multiple-value-list (crate:find-symbol (symbol-name sym) package)))
      :external))

(defun write-symbol (object)
  (let* ((pack nil)
         (name-chars nil)
         (pack-escape nil)
         (name-escape nil)
         (package (crate:symbol-package object))
         (symbol-name (symbol-name object))
         (os *standard-output*)
         (escape *print-escape*))

    ;; if the symbol is in the keyword package, output a colon first
    (if (null package)
        (if *print-gensym*
            (progn
              (push #\# pack)
              (push #\: pack)))
        (if (eq package (crate:find-package :keyword))
            (push #\: pack)
            (multiple-value-bind (symbol status)
                (crate:find-symbol symbol-name (crate:current-package))
              ;; If we can't find a symbol of this name in the current package
              ;; or the symbol we found isn't the same one we want to print,
              ;; then we need to print the package prefix.  JPM.  09/27/01
              (if (or (null status) (not (eq symbol object)))
                  (let ((package-name   (crate:package-name package))
                        (need-bars nil))
                    (dotimes (i (length package-name))
                      (let ((c (elt package-name i)))
                        (if (or (special-char-p c) (lower-case-p c))
                            (setq need-bars t))
                        (push c pack)))
                    (if (and need-bars escape)
                        (progn
                          (setq pack (append '(#\|) pack '(#\|)))
                          (setq pack-escape t)))
                    (if (external-symbol-p object package)
                        (push #\: pack)
                        (progn (push #\: pack) (push #\: pack))))))))

    (let ((need-bars nil))
      (dotimes (i (length symbol-name))
        (let ((c (elt symbol-name i)))
          (if (or (special-char-p c) (lower-case-p c) (whitespace-char-p c))
              (setq need-bars t))
          (push c name-chars)))
      (if (and need-bars escape)
          (progn
            (setq name-chars (append (list #\|) name-chars (list #\|)))
            (setq name-escape t))))

    (setq name-chars (nreverse name-chars))
    (setq pack (nreverse pack))

    (cond
      ((eq *print-case* :downcase)
       (if escape
           (dolist (i pack)
             (%output-char (if pack-escape i (char-downcase i)) os)))
       (dolist (i name-chars)
         (%output-char (if name-escape i (char-downcase i)) os)))
      ((eq *print-case* :capitalize)
       (let ((first-time t))
         (if escape
             (dolist (i pack)
               (if first-time
                   (setq first-time nil)
                   (setq i (char-downcase i)))
               (%output-char (if (or first-time pack-escape)
                                 i
                                 (char-downcase i)) os)))
         (setq first-time t)
         (dolist (i name-chars)
           (if first-time
               (setq first-time nil)
               (setq i (char-downcase i)))
           (%output-char (if (or first-time name-escape)
                             i
                             (char-downcase i)) os))))
      (t
       (if escape (dolist (i pack) (%output-char i os)))
       (dolist (i name-chars) (%output-char i os))))))



(defun write-package (object)
  (let ((*print-escape* nil))
    (cl:write "#<PACKAGE ")
    (write (crate:package-name object) :escape t)
    (cl:write ">")))

(defun struct-print-function-p (object)
  (not (eq (find-class 'structure-object)
           (car
            (c2mop:method-specializers
             (car (c2mop:compute-applicable-methods-using-classes
                   #'cl:print-object
                   (list (class-of object) t))))))))

(defun write-struct (object)
  (if (struct-print-function-p object)
      (cl:write object)
      (let* ((save-print-escape *print-escape*)
             (*print-escape* nil)
             (keyword-package (find-package "KEYWORD")))
        (cl:write "#S(")
        (write-lisp-object (class-name (class-of object)))
        (dolist (slot (mapcar #'c2mop:slot-definition-name
                              (c2mop:class-slots object)))
          (cl:write " ")
          (let ((*print-escape* t))
            (write-lisp-object
             (intern (symbol-name slot) keyword-package)))
          (cl:write " ")
          (let ((*print-escape* save-print-escape))
            (write-lisp-object (slot-value object slot))))
        (cl:write ")"))))


(defun write-array-segment (array index dimension)
  (let* ((need-space nil)
         (max-line-length nil)
         (os *standard-output*)
         (*current-print-level* (+ 1 *current-print-level*))
         (elements (if (array-has-fill-pointer-p array)
                       (length array)
                       (array-dimension array (- dimension 1)))))
    (if *print-pretty*
        (setq max-line-length 80))
    (%output-char #\( os)
    (if (< dimension (array-rank array))
        (dotimes (i elements)
          (write-array-segment array index (+ dimension 1)))
        (dotimes (i elements)
          (if need-space
              (%output-char #\space os)
              #|(if (and max-line-length (> (stream-column os) max-line-length))
              (terpri)
              (%output-char #\space os))|#)

          (if (and (not *print-readably*)
                   *print-length*
                   (>= *print-length* 0)
                   (>= i *print-length*))
              (progn
                (rplaca index (+ (car index) (- elements i)))
                (%output-chars "..." os 0 3)
                (return))
              (write-lisp-object (row-major-aref array (car index))))
          (rplaca index (+ (car index) 1))
          (setq need-space t)))
    (%output-char #\) os)))

(defun write-array (object)
  (let* ((dimensions (array-rank object))
         (os *standard-output*))
    (when (stringp object)
      (cl:write object)
      (return-from write-array))
    (when (and (= dimensions 1)
               (subtypep (array-element-type object) 'BIT))
      (%output-chars "#*" os 0 2)
      (dotimes (i (length object))
        (cl:write (elt object i)))
      (return-from write-array))
    (when (and (not *print-readably*) (not *print-array*))
      (let ((*print-escape* nil))
        (cl:write object)
        (return-from write-array)))
    (%output-char #\# os)
    (when (/= dimensions 1)
      (cl:write dimensions)
      (%output-char #\A os))
    (if (>= dimensions 1)
        (write-array-segment object (list 0) 1)
        (write-lisp-object (list (row-major-aref object 0))))))


;; returns t if the object was output, nil otherwise
(defun output-circular-object (object)
  (let ((n (gethash object *printer-eq-forms*)))
    (when (or (null n) (and (integerp n) (= n 1)))
      (return-from output-circular-object nil))
    (when (and (integerp n) (> n 1))
      (incf *printer-eq-forms-index*)
      (setf (gethash object *printer-eq-forms*)
            (list *printer-eq-forms-index*))
      (%output-char #\# *standard-output*)
      (write-lisp-object *printer-eq-forms-index*)
      (%output-char #\= *standard-output*)
      (return-from output-circular-object nil))
    (when (listp n)
      (%output-char #\# *standard-output*)
      (write-lisp-object (car n))
      (%output-char #\# *standard-output*)
      (return-from output-circular-object t))
    nil))


(defun write-builtin-object (object)
  ;; if we have reached *print-level*, print as a '#' character
  (if (and *print-level*
           (> *print-level* 0)
           (<= *print-level* *current-print-level*))
      (setq object #\#))

  ;; handle circularities if necessary
  (if (and *print-circle*
           (or (consp object)
               (structurep object)
               (typep object '(array t))))
      (if (output-circular-object object)
          (return-from write-builtin-object object)))

  (cond
    ((consp object)             (write-list object))
    ((symbolp object)           (write-symbol object))
    ((and (not (stringp object))
          (arrayp object))      (write-array object))
    ((structurep object)        (write-struct object))
    ((packagep object)          (write-package object))
    (t (cl:write object)))
  object)

(defun write-lisp-object (object)
  (write-builtin-object object))

(defun invalid-object-p (object)
  (declare (ignore object))
  nil)

(defun invalid-object-string (object)
  (declare (ignore object))
  "invalid object")

(defun write (object
              &key (stream          *standard-output*)
                   (escape          *print-escape*)
                   (radix           *print-radix*)
                   (base            *print-base*)
                   (circle          *print-circle*)
                   (pretty          *print-pretty*)
                   (level           *print-level*)
                   (length          *print-length*)
                   (case            *print-case*)
                   (gensym          *print-gensym*)
                   (array           *print-array*)
                   (readably        *print-readably*)
                   (right-margin    *print-right-margin*)
                   (miser-width     *print-miser-width*)
                   (lines           *print-lines*)
                   (pprint-dispatch *print-pprint-dispatch*))


  #|(if (invalid-object-p object)
      (write (invalid-object-string object) :stream stream))|#

  ;; rebind all variables
  (let* ((*standard-output*             stream)
         (*print-escape*                escape)
         (*print-radix*                 radix)
         (*print-base*                  base)
         (*print-circle*                circle)
         (*print-pretty*                pretty)
         (*print-level*                 level)
         (*print-length*                length)
         (*print-case*                  case)
         (*print-gensym*                gensym)
         (*print-array*                 array)
         (*print-readably*              readably)
         (*print-right-margin*          right-margin)
         (*print-miser-width*           miser-width)
         (*print-lines*                 lines)
         (*print-pprint-dispatch*       pprint-dispatch)
         (*current-print-level* 0))

    (if (and *print-circle* (= *current-print-level* 0))
        (let ((*printer-eq-forms* (make-hash-table))
              (*printer-eq-forms-index* 0))
          (search-for-circularities object)
          (write-lisp-object object))
        (write-lisp-object object)))
  object)


(defvar *indent-count* 	0)
(defparameter *indent-tab* 4)
(defparameter *max-line-length*	90)

(defun output-pretty-lambda (s stream)
  (let* ((first (first s))
         (vars (second s))
         (exprs (cddr s)))
    (write first :stream stream)
    (write-char #\Space stream)
    (if (consp vars)
        (output-pretty-list vars stream nil)
        (write vars :stream stream))
    (let ((*indent-count* (+ *indent-count* *indent-tab*)))
      (output-columnar-list exprs stream))))

(defun output-pretty-defining-form (s stream)
  (let ((first (first s))
        (name (second s))
        (vars (third s))
        (exprs (cdddr s)))
    (write first :stream stream)
    (write-char #\Space stream)
    (write name :stream stream)
    (write-char #\Space stream)
    (if (consp vars)
        (output-pretty-list vars stream nil)
        (write vars :stream stream))
    (let ((*indent-count* (+ *indent-count* *indent-tab*)))
      (output-columnar-list exprs stream))))

(defun output-pretty-block (s stream)
  (let ((first (first s))
        (label (second s))
        (exprs (cddr s)))
    (write first :stream stream)
    (write-char #\Space stream)
    (write label :stream stream)
    (let ((*indent-count* (+ *indent-count* *indent-tab*)))
      (output-columnar-list exprs stream))))

(defun output-pretty-let (s stream)
  (let ((first (first s))
        (vars (second s))
        (exprs (cddr s)))
    (if (and (consp vars) (> (list-length vars) 1))
        (progn
          (write first :stream stream)
          (let ((*indent-count* (+ *indent-count* *indent-tab*)))
            (output-pretty-list vars stream t)
            (output-columnar-list exprs stream)))
        (progn
          (write first :stream stream)
          (write-char #\Space stream)
          (write vars :stream stream)
          (let ((*indent-count* (+ *indent-count* *indent-tab*)))
            (output-columnar-list exprs stream))))))

(defun output-pretty-list1-form (s stream)
  (let ((first (first s)))
    (if (consp first)
        (output-pretty-list first stream nil)
        (write-lisp-object first))
    (let ((*indent-count* (+ *indent-count* *indent-tab*)))
      (output-columnar-list (cdr s) stream))))

(defun output-pretty-list2-form (s stream)
  (do* ((p s (cdr p))
        (count 0 (+ count 1)))
       ((not (consp p)))
    (if (> count 0)
        (write-char #\Space stream))
    (if (consp (car p))
        (output-pretty-list (car p) stream nil)
        (write-lisp-object (car p)))
    (when (and (cdr p) (not (consp (cdr p))))
      (write " . " :stream stream :escape nil)
      (write (cdr p) :stream stream))))

(defun output-pretty-list (s &optional (stream *standard-output*)
                                       (need-to-indent nil))
  (let ((first (car s))
        (plength (print-length s)))

    (when need-to-indent
      (write #\Newline :stream stream :escape nil)
      (indent stream))

    ;; check for (quote x) forms and output as 'x
    (if (and (eq first 'quote) (consp (cdr s)) (null (cddr s)))
        (let ((quoted-form (cadr s)))
          (write-char #\' stream)
          (if (consp quoted-form)
              (output-pretty-list quoted-form stream nil)
              (write quoted-form :stream stream))
          (return-from output-pretty-list s)))

    ;; check for (backquote x) forms and output as `x
    #|(if (and (eq first 'cl::backquote) (consp (cdr s)) (null (cddr s)))
        (let ((quoted-form (cadr s)))
          (write-char #\` stream)
          (if (consp quoted-form)
              (output-pretty-list quoted-form stream nil)
              (write quoted-form :stream stream))
          (return-from output-pretty-list s)))|#

    ;; check for (cl::%comma x) forms and output as ,x
    #|(if (and (eq first 'cl::%comma) (consp (cdr s)) (null (cddr s)))
        (let ((quoted-form (cadr s)))
          (write-char #\, stream)
          (if (consp quoted-form)
              (output-pretty-list quoted-form stream nil)
              (write quoted-form :stream stream))
          (return-from output-pretty-list s)))|#

    ;; check for (function x) forms and output as #'x
    (if (and (eq first 'function) (consp (cdr s)) (null (cddr s)))
        (let ((quoted-form (cadr s)))
          (write-char #\# stream)
          (write-char #\' stream)
          (if (consp quoted-form)
              (output-pretty-list quoted-form stream nil)
              (write quoted-form :stream stream))
          (return-from output-pretty-list s)))

    ;; check for (cl::%comma-atsign x) forms and output as ,@x
    #|(if (and (eq first 'cl::%comma-atsign) (consp (cdr s)) (null (cddr s)))
        (let ((quoted-form (cadr s)))
          (write-char #\, stream)
          (write-char #\@ stream)
          (if (consp quoted-form)
              (output-pretty-list quoted-form stream nil)
              (write quoted-form :stream stream))
          (return-from output-pretty-list s)))|#

    (incf *current-print-level*)
    (write-char #\( stream)

    (cond ((and (eq first 'lambda) (consp (cdr s)))
           (output-pretty-lambda s stream))
          ((and (member first '(defun defmacro defgeneric defclass))
                (consp (cdr s)) (consp (cddr s)))
           (output-pretty-defining-form s stream))
          ((and (eq first 'block) (consp (cdr s)))
           (output-pretty-block s stream))
          ((and (member first '(let let*)) (consp (cdr s)))
           (output-pretty-let s stream))
          ((or (consp first) (> (+ plength *indent-count*)
                                *max-line-length*))
           (output-pretty-list1-form s stream))
          (t (output-pretty-list2-form s stream)))

    (write-char #\) stream)
    (decf *current-print-level*)
    s))


;;;
;;;     print-length
;;;     Returns the number of chars required to print the
;;;     passed expression.
;;;
(defun print-length (s)
  (length
   (with-output-to-string (stream)
     (write s :stream stream :pretty nil))))

;;;
;;; indent
;;; Outputs the specified number of spaces.
;;;
(defun indent (stream)
  (dotimes (i *indent-count*)
    (write-char #\Space stream)))

(defun reset-indent () (setq *indent-count* 0))

;;;
;;; output-columnar-list
;;;
(defun output-columnar-list (s stream)
  (dolist (n s)
    (write-char #\Newline stream)
    (indent stream)
    (if (consp n)
        (progn
          (output-pretty-list n stream nil))
        (write n :stream stream)))
  (let ((end (last s)))
    (when (cdr end)
      (write " . " :stream stream)
      (write (cdr end) :stream stream))))


;
;	Common Lisp 'princ' function.
;
(defun princ (object &optional (output-stream *standard-output*))
	(write object :stream output-stream :escape nil))

;
;	Common Lisp 'prin1' function.
;
(defun prin1 (object &optional (output-stream *standard-output*))
	(write object :stream output-stream :escape t))

;
;	Common Lisp 'print' function.
;
(defun print (object &optional (output-stream *standard-output*))
	(write #\Newline :stream output-stream :escape nil)
	(write object :stream output-stream :escape t)
	(write #\Space :stream output-stream :escape nil)
	object)
;
;	Common Lisp 'pprint' function.
;
(defun pprint (object &optional (output-stream *standard-output*))
	(write #\Newline :stream output-stream :escape nil)
	(write object :stream output-stream :escape t :pretty t)
	(values))


;;;
;;;             Common Lisp WRITE-TO-STRING function
;;;
(defun write-to-string (object &rest keys)
  ;;              &key array base case
  ;;              circle escape gensym length level
  ;;              lines miser-width pprint-dispatch
  ;;              pretty radix readably right-margin)
  (with-output-to-string (string)
    (apply 'write object :stream string keys)))

;;;
;;;             Common Lisp PRIN1-TO-STRING function
;;;
(defun prin1-to-string (object)
  (with-output-to-string (string)
    (write object :stream string :escape t)))

;;;
;;;             Common Lisp PRINC-TO-STRING function
;;;
(defun princ-to-string (object)
  (with-output-to-string (string)
    (write object :stream string :escape nil :readably nil)))
