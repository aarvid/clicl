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

(defun format-host (dispatch-char num-args
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

(defun %format-dispatch-host (char num-args)
  (lambda (stream args index atsign-modifier colon-modifier control
           &rest parameters)
    (apply #'format-host
           char num-args
           stream args index atsign-modifier colon-modifier control parameters)))



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
 (%format-dispatch-host #\D 1))

(%set-format-dispatch-func
 #\B
 (%format-dispatch-host #\B 1))

(%set-format-dispatch-func
 #\O
 (%format-dispatch-host #\O 1))

(%set-format-dispatch-func
 #\X
 (%format-dispatch-host #\X 1))

(%set-format-dispatch-func
 #\R
 (%format-dispatch-host #\R 1))

(%set-format-dispatch-func
 #\~
 (%format-dispatch-host #\~ 0))

(%set-format-dispatch-func
 #\%
 (%format-dispatch-host #\% 0))

(%set-format-dispatch-func
 #\P
 (%format-dispatch-host #\P 1))

(%set-format-dispatch-func
 #\Newline
 (%format-dispatch-host #\Newline 0))

(%set-format-dispatch-func
 #\F
 (%format-dispatch-host #\F 1))

(%set-format-dispatch-func
 #\G
 (%format-dispatch-host #\G 1))

(%set-format-dispatch-func
 #\E
 (%format-dispatch-host #\E 1))

