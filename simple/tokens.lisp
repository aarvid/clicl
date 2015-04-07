(cl:in-package #:clicl-reader)

(defun interpret-token (token token-escapes input-stream)
  (convert-according-to-readtable-case token token-escapes)
  (let ((length (length token))
	(sign 1)
	(decimal-mantissa 0)
	(mantissa/numerator 0)
	(denominator 0)
	(fraction-numerator 0)
	(fraction-denominator 1)
	(exponent-sign 1)
	(exponent 0)
	(exponent-marker nil)
	(position-package-marker-1 nil)
	(position-package-marker-2 nil)
	(index -1))
    (tagbody
     start
       (incf index)
       (if (= length index)
	   (return-from interpret-token
	     (crate:intern token))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((eql char #\+)
		    (go sign))
		   ((eql char #\-)
		    (setf sign -1)
		    (go sign))
		   ((digit-char-p char)
		    (setf decimal-mantissa
			  (+ (* decimal-mantissa 10.)
			     (digit-char-p char)))
		    (setf mantissa/numerator
			  (+ (* mantissa/numerator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go decimal-integer))
		   ((digit-char-p char *read-base*)
		    (setf mantissa/numerator
			  (+ (* mantissa/numerator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go integer))
		   ((eql char #\.)
		    (go dot))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     sign
       ;; sign
       (incf index)
       (if (= length index)
	   (return-from interpret-token
	     (crate:intern token))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char)
		    (setf decimal-mantissa
			  (+ (* decimal-mantissa 10.)
			     (digit-char-p char)))
		    (setf mantissa/numerator
			  (+ (* mantissa/numerator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go decimal-integer))
		   ((digit-char-p char *read-base*)
		    (setf mantissa/numerator
			  (+ (* mantissa/numerator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go integer))
		   ((eql char #\.)
		    (go sign-dot))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     dot
       (incf index)
       (if (= length index)
	   (if *consing-dot-allowed-p*
	       (return-from interpret-token
		 *consing-dot*)
	       (error 'invalid-context-for-consing-dot
		      :stream input-stream))
	   (if (= length index)
	   (return-from interpret-token 
	     (* sign mantissa/numerator))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char)
		    (setf fraction-numerator
			  (+ (* fraction-numerator 10)
			     (digit-char-p char)))
		    (setf fraction-denominator
			  (* fraction-denominator 10))
		    (go float-no-exponent))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol))))))
     sign-dot
       ;; sign decimal-point
       (incf index)
       (if (= length index)
	   (return-from interpret-token 
	     (crate:intern token))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char)
		    (setf fraction-numerator
			  (+ (* fraction-numerator 10)
			     (digit-char-p char)))
		    (setf fraction-denominator
			  (* fraction-denominator 10))
		    (go float-no-exponent))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     decimal-integer
       ;; [sign] decimal-digit+
       (incf index)
       (if (= length index)
	   (return-from interpret-token 
	     (* sign mantissa/numerator))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((eql char #\.)
		    (go decimal-integer-final))
		   ((digit-char-p char)
		    (setf decimal-mantissa
			  (+ (* decimal-mantissa 10.)
			     (digit-char-p char)))
		    (setf mantissa/numerator
			  (+ (* mantissa/numerator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go decimal-integer))
		   ((digit-char-p char *read-base*)
		    (setf mantissa/numerator
			  (+ (* mantissa/numerator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go integer))
		   ((eql char #\/)
		    (go ratio-start))
		   ((member char '(#\e #\E #\f #\F #\s #\S #\d #\D #\l #\L))
		    (setf exponent-marker char)
		    (go float-exponent-start))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     decimal-integer-final
       ;; [sign] decimal-digit+ decimal-point
       (incf index)
       (if (= length index)
	   (return-from interpret-token 
	     (* sign decimal-mantissa))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char)
		    (setf fraction-numerator
			  (+ (* fraction-numerator 10)
			     (digit-char-p char)))
		    (setf fraction-denominator
			  (* fraction-denominator 10))
		    (go float-no-exponent))
		   ((member char '(#\e #\E #\f #\F #\s #\S #\d #\D #\l #\L))
		    (setf exponent-marker char)
		    (go float-exponent-start))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     integer
       ;; [sign] digit+
       ;; At least one digit is not decimal.
       (incf index)
       (if (= length index)
	   (return-from interpret-token 
	     (* sign mantissa/numerator))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char *read-base*)
		    (setf mantissa/numerator
			  (+ (* mantissa/numerator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go integer))
		   ((eql char #\/)
		    (go ratio-start))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     ratio-start
       ;; [sign] digit+ /
       (incf index)
       (if (= length index)
	   (return-from interpret-token 
	     (crate:intern token))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char *read-base*)
		    (setf denominator
			  (+ (* denominator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go ratio))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     ratio
       ;; [sign] digit+ / digit+
       (incf index)
       (if (= length index)
	   (return-from interpret-token 
	     (/ mantissa/numerator denominator))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char *read-base*)
		    (setf denominator
			  (+ (* denominator *read-base*)
			     (digit-char-p char *read-base*)))
		    (go ratio))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     float-no-exponent
       ;; [sign] decimal-digit* decimal-point decimal-digit+
       (incf index)
       (if (= length index)
	   (return-from interpret-token 
	     (float (+ mantissa/numerator
		       (/ fraction-numerator fraction-denominator))))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char)
		    (setf fraction-numerator
			  (+ (* fraction-numerator 10)
			     (digit-char-p char)))
		    (setf fraction-denominator
			  (* fraction-denominator 10))
		    (go float-no-exponent))
		   ((member char '(#\e #\E #\f #\F #\s #\S #\d #\D #\l #\L))
		    (setf exponent-marker char)
		    (go float-exponent-start))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     float-exponent-start
       ;; [sign] decimal-digit+ exponent-marker
       ;; or
       ;; [sign] decimal-digit* decimal-point decimal-digit+ exponent-marker
       (incf index)
       (if (= length index)
	   (return-from interpret-token 
	     (crate:intern token))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((eq char #\+)
		    (go float-exponent-sign))
		   ((eq char #\-)
		    (setf exponent-sign -1)
		    (go float-exponent-sign))
		   ((digit-char-p char)
		    (setf exponent
			  (+ (* exponent 10)
			     (digit-char-p char)))
		    (go float-exponent))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     float-exponent-sign
       ;; [sign] decimal-digit+ exponent-marker sign
       ;; or
       ;; [sign] decimal-digit* decimal-point decimal-digit+ exponent-marker sign
       (incf index)
       (if (= length index)
	   (return-from interpret-token 
	     (crate:intern token))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char)
		    (setf exponent
			  (+ (* exponent 10)
			     (digit-char-p char)))
		    (go float-exponent))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     float-exponent
       ;; [sign] decimal-digit+ exponent-marker [sign] digit+
       ;; or
       ;; [sign] decimal-digit* decimal-point decimal-digit+
       ;; exponent-marker [sign] digit+
       (incf index)
       (if (= length index)
	   (return-from interpret-token 
	     (coerce (* (+ mantissa/numerator
			   (/ fraction-numerator
			      fraction-denominator))
			(expt 10 (* exponent-sign exponent)))
		     (ecase exponent-marker
		       ((#\e #\E) 'float)
		       ((#\f #\F)'single-float)
		       ((#\s #\S) 'short-float)
		       ((#\d #\D) 'double-float)
		       ((#\l #\L) 'long-float))))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((digit-char-p char)
		    (setf exponent
			  (+ (* exponent 10)
			     (digit-char-p char)))
		    (go float-exponent))
		   ((eql char #\:)
		    (setf position-package-marker-1 index)
		    (go symbol))
		   (t
		    (go symbol)))))
     symbol
       ;; a sequence of symbols denoting a valid symbol name, except
       ;; that the last character might be a package marker. 
       (incf index)
       (if (= length index)
	   (cond ((null position-package-marker-1)
		  (return-from interpret-token
		    (crate:intern token)))
		 ((null position-package-marker-2)
		  (cond ((= position-package-marker-1 (1- length))
			 (error 'symbol-name-must-not-end-with-package-marker
				:stream input-stream
				:desired-symbol token))
			((= position-package-marker-1 0)
			 (return-from interpret-token
			   (crate:intern (subseq token 1) '#:keyword)))
			(t
			 (multiple-value-bind (symbol status)
			     (crate:find-symbol
			      (subseq token (1+ position-package-marker-1))
			      (subseq token 0 position-package-marker-1))
			   (cond ((null symbol)
				  (error 'symbol-does-not-exist
					 :stream input-stream
					 :desired-symbol token))
				 ((eq status :internal)
				  (error 'symbol-is-not-external
					 :stream input-stream
					 :desired-symbol token))
				 (t
				  (return-from interpret-token symbol)))))))
		 (t
		  (if (= position-package-marker-1 (1- length))
		      (error 'symbol-name-must-not-end-with-package-marker
			     :stream input-stream
			     :desired-symbol token)
		      (return-from interpret-token
			(crate:intern (subseq token (1+ position-package-marker-2))
                                      (subseq token 0 position-package-marker-1))))))
	   (let ((char (aref token index)))
	     (cond ((eq (aref token-escapes index) t)
		    (go symbol))
		   ((eq char #\:)
		    (cond ((null position-package-marker-1)
			   (setf position-package-marker-1 index))
			  ((null position-package-marker-2)
			   (cond ((/= position-package-marker-1 (1- index))
				  (error 'two-package-markers-must-be-adjacent
					 :stream input-stream
					 :desired-symbol token))
				 ((= position-package-marker-1 0)
				  (error 'two-package-markers-must-not-be-first
					 :stream input-stream
					 :desired-symbol token))
				 (t
				  (setf position-package-marker-2 index))))
			  (t
			   (error 'symbol-can-have-at-most-two-package-markers
				  :stream input-stream
				  :desired-symbol token)))
		    (go symbol))
		   (t
		    (go symbol))))))))

