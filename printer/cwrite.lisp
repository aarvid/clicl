(in-package :clicl-printer)

(defconstant left-paren #\( ;; )
	) 
(defconstant right-paren  ;; (
	#\))

(defvar *printer-eq-forms* nil)						;; not exported
(defvar *printer-eq-forms-index* 0)					;; not exported
(defvar *current-print-level* 0)					;; not exported


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

(defun output-pretty-list (object stream)
  (declare (ignore object stream)))

(defun %output-char (char stream)
  (cl:write char :stream stream))

(defun %output-chars (chars stream start stop)
  (cl:write (subseq chars start stop) :stream stream))

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

(defun write-symbol (object)
  (declare (ignore object)))

(defun write-symbol (object)
  (let* ((pack nil)
         (name-chars nil)
         (pack-escape nil)
         (name-escape nil)
         (package (symbol-package object))
         (symbol-name (symbol-name object))
         (os *standard-output*)
         (escape *print-escape*))
	
    ;; if the symbol is in the keyword package, output a colon first
    (if (null package)
        (if *print-gensym*
            (progn
              (push #\# pack)
              (push #\: pack)))
        (if (eq package (find-package :keyword))
            (push #\: pack)
            (multiple-value-bind (symbol status) 
                (find-symbol symbol-name *package*)
              ;; If we can't find a symbol of this name in the current package
              ;; or the symbol we found isn't the same one we want to print,
              ;; then we need to print the package prefix.  JPM.  09/27/01
              (if (or (null status) (not (eq symbol object))) 					
                  (let ((package-name	(package-name package))
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
          (if (or (special-char-p c) (lower-case-p c)(whitespace-char c))
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
       (if escape (dolist (i pack) (%output-char (if pack-escape i (char-downcase i)) os)))
       (dolist (i name-chars) (%output-char (if name-escape i (char-downcase i)) os)))
      ((eq *print-case* :capitalize)
       (let ((first-time t))
         (if escape 
             (dolist (i pack) 
               (if first-time 
                   (setq first-time nil)
                   (setq i (char-downcase i)))
               (%output-char (if (or first-time pack-escape) i (char-downcase i)) os)))
         (setq first-time t)
         (dolist (i name-chars) 
           (if first-time 
               (setq first-time nil)
               (setq i (char-downcase i)))
           (%output-char (if (or first-time name-escape) i (char-downcase i)) os))))
      (t
       (if escape (dolist (i pack) (%output-char i os)))
       (dolist (i name-chars) (%output-char i os))))))

(defun clos-instance-p (object)
  (declare (ignore object)))

(defun write-clos-instance (object)
  (declare (ignore object)))

(defun write-array (object)
  (declare (ignore object)))

(defun write-hashtable (object)
  (declare (ignore object)))

(defun write-struct (object)
  (declare (ignore object)))

(defun write-package (object)
  (declare (ignore object)))



(defun write-builtin-object (object)
  ;; if we have reached *print-level*, print as a '#' character
  (if (and *print-level*
           (> *print-level* 0)
           (<= *print-level* *current-print-level*))
      (setq object #\#))

  ;; handle circularities if necessary
  #|(if (and *print-circle* (or (consp object)
                              (uvectorp object)))
      (if (output-circular-object object)
          (return-from write-builtin-object object)))|#
	
  (cond
    ((consp object)	        (write-list object))
    ((symbolp object)		(write-symbol object))
    ((clos-instance-p object)   (write-clos-instance object))
    ((and (not (stringp object))
          (arrayp object))	(write-array object))
    ((structurep object)	(write-struct object))
    ((hash-table-p object)	(write-hashtable object))
    ((packagep object)		(write-package object))
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
