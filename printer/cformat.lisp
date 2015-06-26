(in-package :clicl-printer)

(defmacro ensure-char (char default)
  `(if ,char
       (if (integerp ,char)
           (code-char ,char)
           ,char)
       ,default))

(defun format (dest control-string &rest arguments)
  (let ((return-value nil))
    ;; check for dest equal to t or nil
    (cond
      ((null dest)
       (progn
         (setf dest (make-string-output-stream))
         (setf return-value dest)))
      ((eq dest t) (setf dest *standard-output*)))
    (if (stringp dest)
        (with-output-to-string (stream dest)
          (if (functionp control-string)
              (apply control-string stream arguments)
              (catch '%format-up-and-out
                (%format-list stream control-string arguments))))
        (catch '%format-up-and-out
          (if (functionp control-string)
              (apply control-string dest arguments)
              (%format-list dest control-string arguments))))
    (if return-value (get-output-stream-string return-value))))


;;;
;;; This is like FORMAT, but for use by FORMATTER.
;;;
(defun format-internal (dest control-string &rest arguments)
  (let ((arg-index
          (catch '%format-up-and-out
            (%format-list dest control-string arguments))))
    (nthcdr arg-index arguments)))

(defun %format-list (dest control-string arguments &optional (arg-index 0))
  ;; scan control string and dispatch to output functions
  (do ((index 0)
       (orig-arg-index arg-index)
       (length (length control-string))
       (atsign-modifier nil nil)
       (colon-modifier nil nil)
       dispatch-func
       (parameters nil)
       control
       args-used
       char)
      ((>= index length) (- arg-index orig-arg-index))
    (setf char (char control-string index))
    (if (char= char #\~)
        ;; process directive
        (progn
          ;; get parameters
          (incf index)
          (multiple-value-setq (parameters index args-used)
            (%get-params control-string index arguments arg-index))
          (incf arg-index args-used)

          ;; check for modifiers
          (dotimes (i 2)
            (if (>= index length) (return))
            (setq char (char control-string index))
            (cond ((char= char #\@)(setq atsign-modifier t))
                  ((char= char #\:)(setq colon-modifier t))
                  (t (return)))
            (incf index))

          ;; the next character should be the format
          ;; directive character
          (if (>= index length)
              (error "Invalid format directive: ~A" control-string))
          (setq char (char control-string index))
          (incf index)
          (setf dispatch-func
                (%get-format-dispatch-func char))
          (if (null dispatch-func)
              (error "Invalid format directive : character ~S in control string ~S"
                     char control-string))
          (setq control (list control-string index))
          (setq arg-index
                (apply dispatch-func
                       dest
                       arguments arg-index
                       atsign-modifier colon-modifier
                       control
                       parameters))
          (setq index (cadr control)))

        ;; just output the character
        (progn
          (write-char char dest)
          (incf index)))))


;;;
;;;   Returns two values: the list of params found and the
;;;    updated index.
;;;
(defun %get-params (control-string index arguments arg-index &aux (params nil)(args-used 0))
  (do ((int nil nil)
       c
       (length (length control-string)))
      ((>= index length))
    (if (char= (char control-string index) #\Newline)
        (return))
    (if (eql (char control-string index) #\')
        (progn
          (setq int (char control-string (+ index 1)))
          (incf index 2))
        (if (eql (char-upcase (char control-string index)) #\V)
            (progn
              (setq int (elt arguments arg-index))
              (incf arg-index)
              (incf args-used)
              (incf index))
            (if (eql (char control-string index) #\#)
                (progn
                  (setq int (- (length arguments) arg-index))
                  (incf index))
                (multiple-value-setq (int index)
                  (parse-integer control-string :start index
                                                :junk-allowed t)))))
    (setq c (char control-string index))
    (if int
        (push int params)
        (if (char= c #\,)
            (push nil params)))
    (if (char= c #\,) (incf index) (return)))
  (values (nreverse params) index args-used))



(defun %format-integer (stream int radix atsign-modifier colon-modifier
                        mincol padchar commachar)

  ;; initialize defaults
  (unless mincol (setq mincol 0))
  (setq padchar (ensure-char padchar #\Space))
  (setq commachar (ensure-char commachar #\,))

  (let ((*print-base* radix)
        (*print-radix* nil)
        s
        (length 0)
        sign)

    (if (and atsign-modifier (plusp int))
        (progn (setf sign #\+) (incf length))
        (if (minusp int)
            (progn (setf sign #\-) (incf length) (setf int (- int)))))

    (setq s (with-output-to-string (x) (princ int x)))
    (incf length (length s))
    (if colon-modifier
        (incf length (truncate (1- (length s)) 3)))
    (if (< length mincol)
        (dotimes (i (- mincol length))
          (write-char padchar stream)))

    (if sign (write-char sign stream))

    (if colon-modifier
        (dotimes (i (length s))
          (write-char (char s i) stream)
          (let* ((digits-left (- (length s) (1+ i)))
                 (digit-pos (mod digits-left 3)))
            (if (and (zerop digit-pos) (plusp digits-left))
                (write-char commachar stream))))
        (princ s stream))))


  



(defun %format-cardinal-number (int stream)
  (if (zerop int) (return-from %format-cardinal-number (princ "zero" stream)))
  (if (minusp int) 
      (progn (princ "negative " stream) (setq int (- int))))
  (cond
    ((< int 20)
     (princ (nth int '("zero" "one" "two" "three" "four" "five" 
                       "six" "seven" "eight" "nine" "ten"
                       "eleven" "twelve" "thirteen" "fourteen" "fifteen"
                       "sixteen" "seventeen" "eighteen" "nineteen")) 
            stream))
    ((< int 100)
     (princ (nth (- (truncate int 10) 2) '("twenty" "thirty" "forty"
                                           "fifty" "sixty" "seventy"
                                           "eighty" "ninety"))
            stream)
     (if (plusp (mod int 10)) 
         (progn 
           (write-char #\- stream)
           (%format-cardinal-number (mod int 10) stream))))
    ((< int 1000)
     (%format-cardinal-number (truncate int 100) stream)
     (princ " hundred" stream)
     (if (plusp (mod int 100))
         (progn  
           (write-char #\Space stream)              
           (%format-cardinal-number (mod int 100) stream))))
    ((< int 1000000)
     (%format-cardinal-number (truncate int 1000) stream)
     (princ " thousand" stream)
     (if (plusp (mod int 1000))
         (progn  
           (write-char #\Space stream)              
           (%format-cardinal-number (mod int 1000) stream))))
    #|
                        ((< int 1000000000)
                         (%format-cardinal-number (truncate int 1000000) stream)
                         (princ " million" stream)
                         (if (plusp (mod int 1000000))
                                (progn  
                                        (write-char #\Space stream)              
                                        (%format-cardinal-number (mod int 1000000) stream))))
|#
                        (t (princ "billions"))))


(defun %format-ordinal-number (int stream)
  (declare (ignore int))
  (princ "Sorry" stream))

(defun %format-roman-numeral (int stream)
  (declare (ignore int))
  (princ "Sorry" stream))

(defun %format-old-roman-numeral (int stream)
  (declare (ignore int))
  (princ "Sorry" stream))


;;; Format dispatch functions take a stream, argument list,
;;; @-modifier and :-modifier arguments, followed by any passed
;;; parameters. Any passed parameters which are nil should be
;;; assumed to be requesting the default. The dispatch functions
;;; should return the remaining argument list (missing the
;;; arguments that they processed.
;;;

(defvar *format-functions* #256())

(defun %set-format-dispatch-func (char func)
  (let ((index (char-code (char-upcase char))))
    (setf (elt *format-functions* index) func)))

(defun %get-format-dispatch-func (char)
  (let ((index (char-code (char-upcase char))))
    (elt *format-functions* index)))


(%set-format-dispatch-func
 #\A
 #'(lambda (stream args index atsign-modifier colon-modifier control
            &optional mincol colinc
                      minpad padchar)
     (declare (ignore control))
     (setq args (nthcdr index args))
     (if (null args)
         (error "Not enough args for ~AA format directive" #\~))

     ;; initialize defaults
     (unless mincol (setq mincol 0))
     (unless colinc (setq colinc 1))
     (unless minpad (setq minpad 0))
     (setq padchar (ensure-char padchar #\Space))

     (let ((*print-escape* nil)
           (arg (car args)))
       (if (and (null arg) colon-modifier)
           (setq arg "()"))
       (let* ((s (with-output-to-string (x) (princ arg x)))
              (length (length s)))
         (if atsign-modifier
             ;; needto output to string to insert padding in front
             (progn
               (dotimes (i minpad) (write-char padchar stream))
               (incf length minpad)
               (do ()
                   ((>= length mincol))
                 (dotimes (i colinc) (write-char padchar stream))
                 (incf length colinc))
               (princ s stream))
             (progn
               (princ s stream)
               (dotimes (i minpad) (write-char padchar stream))
               (incf length minpad)
               (do ()
                   ((>= length mincol))
                 (dotimes (i colinc) (write-char padchar stream))
                 (incf length colinc))))))
     (1+ index)))


(%set-format-dispatch-func
 #\S
 #'(lambda (stream args index atsign-modifier colon-modifier control
            &optional mincol colinc
                      minpad padchar)
     (declare (ignore control))
     (setq args (nthcdr index args))
     (if (null args)
         (error "Not enough args for ~AS format directive" #\~))

     ;; initialize defaults
     (unless mincol (setq mincol 0))
     (unless colinc (setq colinc 1))
     (unless minpad (setq minpad 0))
     (setq padchar (ensure-char padchar #\Space))

     (let ((*print-escape* t)
           (arg (car args)))
       (if (and (null arg) colon-modifier)
           (setq arg "()"))
       (let* ((s (with-output-to-string (x) (prin1 arg x)))
              (length (length s)))
         (if atsign-modifier
            ;; need to output to string to insert padding in front
            (progn
              (dotimes (i minpad) (write-char padchar stream))
              (incf length minpad)
              (do ()
                  ((>= length mincol))
                (dotimes (i colinc) (write-char padchar stream))
                (incf length colinc))
              (princ s stream))
            (progn
              (princ s stream)
              (dotimes (i minpad) (write-char padchar stream))
              (incf length minpad)
              (do ()
                  ((>= length mincol))
                (dotimes (i colinc) (write-char padchar stream))
                (incf length colinc))))))
     (1+ index)))


(%set-format-dispatch-func
 #\D
 #'(lambda (stream args index atsign-modifier colon-modifier control
            &optional mincol padchar commachar)
     (block nil
       (let ((save-args args))
         (setq args (nthcdr index args))
         (if (null args)
             (error "Not enough args for ~~D format directive"))

         ;; if not an integer use ~A output
         (if (not (integerp (car args)))
             (let ((*print-base* 10))
               (return (funcall (%get-format-dispatch-func #\A)
                                stream save-args index atsign-modifier
                                colon-modifier control mincol 1 padchar
                                commachar))))

         (%format-integer stream (car args) 10 atsign-modifier colon-modifier
                          mincol padchar commachar)
         (1+ index)))))


(%set-format-dispatch-func
 #\B
 #'(lambda (stream args index atsign-modifier colon-modifier control
            &optional mincol padchar commachar)
     (declare (ignore control))
     (block nil
       (setq args (nthcdr index args))
       (if (null args)
           (error "Not enough args for ~AB format directive" #\~))

       ;; if not an integer use ~A output
       (if (not (integerp (car args)))
           (let ((*print-base* 2))
             (return (apply (%get-format-dispatch-func #\A)
                            stream args atsign-modifier
                            colon-modifier mincol nil nil padchar))))

       (%format-integer stream (car args) 2 atsign-modifier colon-modifier
                        mincol padchar commachar)
       (1+ index))))

(%set-format-dispatch-func
 #\O
 #'(lambda (stream args index atsign-modifier colon-modifier control
            &optional mincol padchar commachar)
     (declare (ignore control))
     (block nil
       (setq args (nthcdr index args))
       (if (null args)
           (error "Not enough args for ~AO format directive" #\~))

       ;; if not an integer use ~A output
       (if (not (integerp (car args)))
           (let ((*print-base* 8))
             (return (apply (%get-format-dispatch-func #\A)
                            stream args atsign-modifier
                            colon-modifier mincol nil nil padchar))))

       (%format-integer stream (car args) 8 atsign-modifier colon-modifier
                        mincol padchar commachar)
       (1+ index))))

(%set-format-dispatch-func
 #\X
 #'(lambda (stream args index atsign-modifier colon-modifier control
            &optional mincol padchar commachar)
     (declare (ignore control))
     (block nil
       (setq args (nthcdr index args))
       (if (null args)
           (error "Not enough args for ~AX format directive" #\~))

       ;; if not an integer use ~A output
       (if (not (integerp (car args)))
           (let ((*print-base* 16))
             (return (apply (%get-format-dispatch-func #\A)
                            stream args atsign-modifier
                            colon-modifier mincol nil nil padchar))))

       (%format-integer stream (car args) 16 atsign-modifier colon-modifier
                        mincol padchar commachar)
       (1+ index))))

(%set-format-dispatch-func
 #\R
 #'(lambda (stream args index atsign-modifier colon-modifier control
            &optional radix mincol padchar commachar)
     (declare (ignore control))
     (block nil
       (setq args (nthcdr index args))
       (if (null args)
           (error "Not enough args for ~AR format directive" #\~))

       (if radix
           ;; if not an integer use ~A output
           (progn
             (if (not (integerp (car args)))
                 (let ((*print-base* radix))
                   (return (apply (%get-format-dispatch-func #\A)
                                  args atsign-modifier
                                  colon-modifier mincol nil nil padchar))))
             (unless (and (plusp radix) (<= radix 36))
               (error "Invalid radix specified: ~A" radix))
             (%format-integer stream (car args) radix atsign-modifier colon-modifier
                              mincol padchar commachar))
           (progn
             (if (not (integerp (car args)))
                 (return (apply (%get-format-dispatch-func #\A)
                                args atsign-modifier
                                colon-modifier mincol nil nil padchar)))
             (cond
               ((and atsign-modifier colon-modifier)
                (%format-old-roman-numeral (car args) stream))
               (atsign-modifier (%format-roman-numeral (car args) stream))
               (colon-modifier (%format-ordinal-number (car args) stream))
               (t (%format-cardinal-number (car args) stream)))))
       (1+ index))))

(%set-format-dispatch-func
 #\~
 #'(lambda (stream args index atsign-modifier colon-modifier control
            &optional num)
     (declare (ignore args atsign-modifier colon-modifier control))
     (unless num (setq num 1))
     (dotimes (i num)
       (write-char #\~ stream))
     index))

(%set-format-dispatch-func
 #\%
 #'(lambda (stream args index atsign-modifier colon-modifier control
            &optional num)
     (declare (ignore args atsign-modifier colon-modifier control))
     (unless num (setq num 1))
     (dotimes (i num)
       (write-char #\Newline stream))
     index))

(%set-format-dispatch-func
 #\P
 #'(lambda (stream args index atsign-modifier colon-modifier control)
     (declare (ignore control))
     (when colon-modifier
          (decf index)
          (if (< index 0)
              (error "No preceding argument for :P modifer to format string")))
     (setq args (nthcdr index args))
     (if (not (eql (car args) 1))
         (if atsign-modifier
             (write-string "ies" stream)
             (write-char #\s stream))
         (if atsign-modifier
             (write-string "y" stream)))
     (1+ index)))

(%set-format-dispatch-func
 #\Newline
 #'(lambda (stream args index atsign-modifier colon-modifier control )
     (declare (ignore stream args atsign-modifier colon-modifier control))
     index))



(defun format-down (dispatch-char num-args
                    stream args arg-index atsign-modifier colon-modifier control
            &rest parameters)
  (declare (ignore control))
  (let ((control-string
          (cl:format nil
           "~~~{~a~^,~}~:[~;:~]~:[~;@~]~c"
           (mapcar (lambda (p)
                     (cond ((null p) "")
                           ((characterp p)
                            (cl:format nil "'~c" p))
                           (t p)))
                   parameters)
           atsign-modifier colon-modifier dispatch-char))
        (nargs (subseq args arg-index (+ arg-index num-args))))
    (apply #'cl:format stream control-string nargs)
    (+ arg-index num-args)))

(%set-format-dispatch-func
 #\Z
 #'(lambda (stream args index atsign-modifier colon-modifier control &rest parameters)
     (apply #'format-down
            #\Z 1
            stream args index atsign-modifier colon-modifier control parameters)))


(%set-format-dispatch-func
 #\F
 #'(lambda (stream args index atsign-modifier colon-modifier control &rest parameters)
     (apply #'format-down
            #\F 1
            stream args index atsign-modifier colon-modifier control parameters)))
