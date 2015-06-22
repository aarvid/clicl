(in-package :clicl-printer)



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
     (setq padchar (if padchar (if (integerp padchar)
                                   (code-char padchar)
                                   padchar)
                       #\Space))

     (let ((*print-escape* nil)
           (arg (car args)))
       (if (and (null arg) colon-modifier)
           (setq arg "()"))
       (if atsign-modifier
           ;; needto output to string to insert padding in front
           (let ((s (with-output-to-string (x) (princ arg x)))
                 length)
             (dotimes (i minpad) (write-char padchar stream))
             (setq length (length s))
             (incf length minpad)
             (do ()
                 ((>= length mincol))
               (dotimes (i colinc) (write-char padchar stream))
               (incf length colinc))
             (princ s stream))
           (let (length (start-pos (cl:file-position stream)))
             (princ arg stream)
             (setq length (- (cl:file-position stream) start-pos))
             (if (< length 0) (setq length 0))
             (dotimes (i minpad) (write-char padchar stream))
             (incf length minpad)
             (do ()
                 ((>= length mincol))
               (dotimes (i colinc) (write-char padchar stream))
               (incf length colinc)))))
     (1+ index)))
