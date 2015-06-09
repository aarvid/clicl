(in-package :clicl-printer)

(defvar *printer-eq-forms* nil)						;; not exported
(defvar *printer-eq-forms-index* 0)					;; not exported
(defvar *current-print-level* 0)					;; not exported


(defun structurep (object)
  (typep object 'structure-object))
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
    (if (= n 0)
        (cond ((consp object)
               (search-for-circularities (car object))
               (search-for-circularities (cdr object)))
              ((structurep object)
               (dotimes (i (uvector-num-slots object))
                 (search-for-circularities (uref object (+ i 1)))))                  
              (t (let ((size (apply '* (array-dimensions object))))
                   (dotimes (i size)
                     (search-for-circularities (row-major-aref object i)))))))))


(defun write-lisp-object (object)
    (write-builtin-object object))
    
(defun invalid-object-p (object)
  (declare (ignore object))
  nil)

(defun invalid-object-string (object)
  (declare (ignore object))
  "invalid object")

(defun write (object 
              &key (stream		*standard-output*)
                   (escape		*print-escape*)
                   (radix			*print-radix*)
                   (base			*print-base*)
                   (circle		*print-circle*)
                   (pretty		*print-pretty*)
                   (level			*print-level*)
                   (length		*print-length*)
                   (case			*print-case*)
                   (gensym		*print-gensym*)
                   (array			*print-array*)
                   (readably		*print-readably*)
                   (right-margin	*print-right-margin*)
                   (miser-width	*print-miser-width*)
                   (lines			*print-lines*)
                   (pprint-dispatch *print-pprint-dispatch*))

    
  (if (invalid-object-p object)
      (write (invalid-object-string object) :stream stream))

  ;; rebind all variables
  (let* ((*standard-output*		stream)
         (*print-escape*			escape)
         (*print-radix*			radix)
         (*print-base*			base)
         (*print-circle*			circle)
         (*print-pretty*			pretty)
         (*print-level*			level)
         (*print-length*			length)
         (*print-case*			case)
         (*print-gensym*			gensym)
         (*print-array*			array)
         (*print-readably*		readably)
         (*print-right-margin*	right-margin)
         (*print-miser-width*		miser-width)
         (*print-lines*			lines)
         (*print-pprint-dispatch* pprint-dispatch)
         (*current-print-level* 0))
    
    (if (and *print-circle* (= *current-print-level* 0))
        (let ((*printer-eq-forms* (make-hash-table))
              (*printer-eq-forms-index* 0))
          (search-for-circularities object)	
          (write-lisp-object object))
        (write-lisp-object object)))
  object)
