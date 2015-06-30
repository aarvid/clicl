;;;;    -------------------------------
;;;;    Copyright (c) Corman Technologies Inc.
;;;;    See LICENSE.txt for license information.
;;;;    -------------------------------
;;;;
;;;;    File:           format.lisp
;;;;    Contents:       Common Lisp FORMAT function implementation
;;;;    Author:         Roger Corman
;;;;    History:        ??/??/96 RGC Created.
;;;;                    09/03/99 RGC Implemented ~P modifiers (: and @).
;;;;                    01/31/01 RGC Added Chris Double's fix for FORMAT ~@[...] directive
;;;;                    12/14/01 RGC Fixed a bug with ~[ expression (reported by jmarshall)
;;;;                    29/06/15 AAP modifications for clicl/crate 


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


(%set-format-dispatch-func
 #\?
 #'(lambda (stream args index atsign-modifier colon-modifier control
            &optional width digits scale overflow-char padchar)
     (declare (ignore atsign-modifier colon-modifier control
                      width digits scale overflow-char padchar))
     (setq args (nthcdr index args))
     (if (or (null args) (null (cdr args)))
         (error "Not enough args for ~~? format directive"))
     (let ((str (make-string-output-stream)))
       (%format-list str (car args) (cadr args))
       (write-string (get-output-stream-string str) stream))
     (+ index 2)))


(defun find-matching-right-brace (string startpos)
  (do ((i startpos (+ i 1))
       (endpos (length string))
       (nesting 1)
       tilda-pos)
      ((= i (- endpos 1)))
    (when (char= (char string i) #\~)
      (setf tilda-pos i)
      (incf i)
      (let ((c (char string i)))
        ;; skip past any modifier characters
        (do ()
            ((or (>= i (- endpos 1))
                 (not (member c '(#\@ #\:)))))   ;; any others allowed here?
          (incf i)
          (setf c (char string i)))
        (cond ((char= c #\{) (incf nesting))
              ((char= c #\}) (decf nesting)
               (if (zerop nesting)
                   (return tilda-pos))))))))

(%set-format-dispatch-func
 #\{
 #'(lambda (stream args index atsign-modifier colon-modifier control)
     (block nil
       (setq args (nthcdr index args))
       (unless args
         (error "Not enough args for ~~{ format directive"))
       (unless (or (listp (car args)) atsign-modifier)
         (error "Invalid format argument--should be a list"))

       (let ((end-brace-index (find-matching-right-brace (car control)
                                                         (cadr control)))
             string)
         (if end-brace-index
             (setq string (subseq (car control) (cadr control) end-brace-index))
             (error "Missing ~~} following ~~{ in format string"))
         (setf (cadr control) (+ 2 end-brace-index))
         (cond
           ((and colon-modifier atsign-modifier)
            (return
              (do ((arg-index 0))
                  ((>= arg-index (length args)) (+ index arg-index))
                (%format-list stream string (nth arg-index args))
                (incf arg-index))))
           (colon-modifier
            (return
              (do ((arg-index 0))
                  ((>= arg-index (length (car args))) (1+ index))
                (%format-list stream string (nth arg-index (car args)))
                (incf arg-index))))
           (atsign-modifier
            (return
              (do ((arg-index 0))
                  ((>= arg-index (length args)) (+ index arg-index))
                (incf arg-index
                      (%format-list stream string (nthcdr arg-index args))))))
           (t
            (catch '%format-up-and-out
              (do ((arg-index 0))
                  ((>= arg-index (length (car args))) (1+ index))
                (incf arg-index
                      (%format-list stream string
                                    (nthcdr arg-index (car args))))))
            (1+ index)))))))

;; case conversion
(defun paren-dispatch-func (stream args index atsign-modifier colon-modifier control)
  (block nil
    (setq args (nthcdr index args)) ;; skip unnecessary arguments

    ;; collect the characters up until a closing parentheses
    (let ((close-paren-index (search "~)" (car control) :start2 (cadr control)))
          string
          (string-stream (make-string-output-stream)))
      (if close-paren-index
          (setq string (subseq (car control) (cadr control) close-paren-index))
          (error "Missing ~~) following ~~( in format string"))
      (setf (cadr control) (+ 2 close-paren-index))
      (setf index
            (catch '%format-up-and-out
              (%format-list string-stream string args)))
      (setq string (get-output-stream-string string-stream))
      (cond
        ((and colon-modifier atsign-modifier)
         (progn
           (setq string (string-upcase string))
           (write-string string stream)
           (return index)))
        (colon-modifier
         (progn
           (setq string (string-capitalize string))
           (write-string string stream)
           (return index)))
        ;; need to fix this to only capitalize the first word
        (atsign-modifier
         (progn
           (setq string (string-capitalize string))
           (write-string string stream)
           (return index)))
        (t
         (progn
           (setq string (string-downcase string))
           (write-string string stream)
           (return index)))))))

(%set-format-dispatch-func
 #\(
 'paren-dispatch-func)

(%set-format-dispatch-func
 #\^
 #'(lambda (stream args index atsign-modifier colon-modifier control)
     (declare (ignore stream atsign-modifier colon-modifier control))
     (setq args (nthcdr index args))
     (unless args (throw '%format-up-and-out index))
     index))


;; locates either ~; or ~:;
;; returns its position, and true if a : modifier was found, nil otherwise
(defun find-format-option-separators (string startpos)
  (do ((i startpos (+ i 1))
       (endpos (length string))
       tilda-pos
       (colon-modifier nil))
      ((> i (- endpos 2)))
    (when (char= (char string i) #\~)
      (setf tilda-pos i)
      (incf i)
      (let ((c (char string i)))
        ;; skip past any modifier characters
        (when (char= c #\:)
          (setf colon-modifier t)
          (incf i)
          (setf c (char string i)))
        (if (char= c #\;)
            (return (values tilda-pos colon-modifier)))))))

;;; Returns the index of the next format directive, or NIL if none found.
;;; The index returned will be of the actual character i.e. "~A" will return 1 (the index of #\A)
;;;
(defun find-format-directive (control-string index)
  (let ((length (length control-string)))
    (do* ((ch (char control-string index) (char control-string index)))
         ((char= ch #\~))
      (incf index)
      (if (= index length) (return-from find-format-directive nil)))
    (incf index)    ;; skip tilda character

    ;; scan past parameters
    (do* ((ch (char control-string index) (char control-string index)))
         ((not (or (digit-char-p ch)(char= ch #\,))))
      (incf index)
      (if (= index length) (return-from find-format-directive nil)))

    ;; scan past modifiers
    (do* ((ch (char control-string index) (char control-string index)))
         ((not (or (char= ch #\:)(char= ch #\@))))
      (incf index)
      (if (= index length) (return-from find-format-directive nil)))

    (if (= index length) nil index)))

;;; Using the string between the ~[ and ~] or ~< and ~>, return a list of the
;; control strings separated by ~;
;;;
(defun %expr-list (string)
  (let ((position 0)
        (size (length string))
        (substrs '())
        (start 0)
        (nesting-stack '())
        (colon-modifier-active nil))
    (do ()
        ((= position size))
      (setq position (find-format-directive string position))
      (unless position
        (let ((colon-modifier (and (> size 0)
                                   (char= (char string (- size 1))
                                          #\:))))
          (push (list (subseq string start size)
                      colon-modifier-active)
                substrs))
        (return))
      (let ((ch (char string position)))
        (cond
          ((or (char= ch #\[) (char= ch #\<) (char= ch #\{))
           (push ch nesting-stack))
          ((char= ch #\])
           (unless (char= (pop nesting-stack) #\[)
             (error "Error in control string: encountered an unexpected #\] character")))
          ((char= ch #\})
           (unless (char= (pop nesting-stack) #\{)
             (error "Error in control string: encountered an unexpected #\} character")))
          ((char= ch #\>)
           (unless (char= (pop nesting-stack) #\<)
             (error "Error in control string: encountered an unexpected #\> character")))
          ((and (char= ch #\;) (null nesting-stack))
           (let ((colon-modifier (and (> position 0)
                                      (char= (char string (- position 1)) #\:))))
             (push (list (subseq string start
                                 (- position (+ 1 (if colon-modifier 1 0))))
                         colon-modifier-active)
                   substrs)
             (setq start (+ position 1))
             (setf colon-modifier-active colon-modifier))))))
    (nreverse substrs)))

(defun format-choose-selection-from-list (selection-list index)
  (if (< index (length selection-list))
      (car (nth index selection-list))
      (progn  ;; look for an else selector (indicated by ~:;)
        (dolist (x selection-list)
          (if (eq (cdr x) 't)
              (return-from format-choose-selection-from-list (car x))))
        (error "Not enough items in the FORMAT selector list.Items = ~S, index = ~D"
               selection-list index))))
;;
;; Look for terminating ~] following ~[. Watch for modifiers, nesting issues.
;;
(defun find-matching-right-bracket (string startpos)
  (do ((i startpos (+ i 1))
       (endpos (length string))
       (nesting 1)
       tilda-pos)
      ((= i (- endpos 1)))
    (when (char= (char string i) #\~)
      (setf tilda-pos i)
      (incf i)
      (let ((c (char string i)))
        ;; skip past any modifier characters
        (do ()
            ((or (>= i (- endpos 1))
                 (not (member c '(#\@ #\: #\#)))))       ;; any others allowed here?
          (incf i)
          (setf c (char string i)))
        (cond ((char= c #\[)(incf nesting))
              ((char= c #\])(decf nesting)(if (zerop nesting)(return tilda-pos))))))))


;;;
;;; We found a ~<, so now we search for a matching ~>
;;;

(defun find-matching-greater-than (string position)
  (let ((size (length string))
        ;;(substrs '())
        (nesting-stack '()))
    (do ()
        ((= position size))
      (setq position (find-format-directive string position))
      (unless position
        (return-from find-matching-greater-than nil))
      (let ((ch (char string position)))
        (cond
          ((or (char= ch #\[) (char= ch #\<) (char= ch #\{))
           (push ch nesting-stack))
          ((char= ch #\])
           (unless (char= (pop nesting-stack) #\[)
             (error "Error in control string: encountered an unexpected #\] character")))
          ((char= ch #\})
           (unless (char= (pop nesting-stack) #\{)
             (error "Error in control string: encountered an unexpected #\} character")))
          ((and (char= ch #\>) nesting-stack)
           (unless (char= (pop nesting-stack) #\<)
             (error "Error in control string: encountered an unexpected #\> character")))
          ((and (char= ch #\>) (null nesting-stack))
           (do ((ch (char string position)
                    (char string position)))
               ((char= ch #\~)
                (return-from find-matching-greater-than position))
             (decf position))))))))

;; conditional expressions
;; Note: despite some work, these expressions still do not nest correctly.
;; The inner ~[~] semicolons will be used as separators for the outer
;; braces. -RGC 10/1/99
(%set-format-dispatch-func
 #\[
 #'(lambda (stream args index atsign-modifier colon-modifier control
            &optional (num nil))
;       (setq args (nthcdr index args));; skip unnecessary arguments
     (if (and (< (- (length args) index) 1) (null num))
         (error "Not enough args for ~~[ format directive"))
     ;; collect the characters up until a closing brace
     (let ((close-brace-index
             (find-matching-right-bracket (car control) (cadr control)))
           string
           conditional-exprs
           selector
           (string-stream (make-string-output-stream)))
       (if close-brace-index
           (setq string (subseq (car control) (cadr control) close-brace-index))
           (error "Missing ~~] following ~~[ in format string"))
       (setf (cadr control) (+ 2 close-brace-index))
       (setf conditional-exprs (%expr-list string))
       (setf selector (or num (nth index args)))
       (unless (or num atsign-modifier)
         (incf index))
       (cond
         ((and colon-modifier atsign-modifier)
          (error "~~:@[ not allowed in format control string"))
         (atsign-modifier
          (if selector
              (let ((ctstring
                      (format-choose-selection-from-list conditional-exprs 0)))
                (incf index (%format-list string-stream ctstring args index))
                (setq string (get-output-stream-string string-stream))
                (write-string string stream))
              (incf index)))
         (colon-modifier
          (let ((ctstring
                  (format-choose-selection-from-list conditional-exprs
                                                     (if selector 1 0))))
          ; (format t "ctstring=~S, args=~A~%" ctstring args)(force-output)
            (incf index (%format-list string-stream ctstring args index))
            (setq string (get-output-stream-string string-stream))
            (write-string string stream)))
         (t
          (let ((ctstring
                  (format-choose-selection-from-list conditional-exprs
                                                     selector)))
            (incf index (%format-list string-stream ctstring args index))
            (setq string (get-output-stream-string string-stream))
            (write-string string stream))))
       index)))


(%set-format-dispatch-func
 #\&
 (%format-dispatch-host #\& 0))

(%set-format-dispatch-func
 #\|
 (%format-dispatch-host #\| 0))

(%set-format-dispatch-func
 #\Newline
 (%format-dispatch-host #\Newline 0))

(%set-format-dispatch-func
 #\T
 (%format-dispatch-host #\T 0))

(%set-format-dispatch-func
 #\C
 (%format-dispatch-host #\C 1))


(%set-format-dispatch-func
 #\*
 #'(lambda (stream args index atsign-modifier colon-modifier control
            &optional num)
     (declare (ignore stream args control))
     (block nil
       (unless num (if atsign-modifier (setq num 0) (setq num 1)))
       (if atsign-modifier
           (return num))
       (if colon-modifier (return (- index num)))
       (return (+ index num)))))


(%set-format-dispatch-func
 #\W
 #'(lambda (stream args index atsign-modifier colon-modifier control)
     (declare (ignore control))
     (let ((*print-pretty* *print-pretty*)
           (*print-level* *print-level*)
           (*print-length* *print-length*))
       (if colon-modifier
           (setf *print-pretty* t))
       (if atsign-modifier
           (setf *print-length* nil *print-level* nil))
       (setq args (nthcdr index args))
       (write (car args) :stream stream)
       (1+ index))))


(defun justify-strings (stream strings mincol colinc minpad padchar)
  (let ((total-chars 0)
        (padding 0)
        (num-strs (length strings))
        (str (car strings)))
    (dolist (x strings) (incf total-chars (length x)))
    (setq padding (- mincol total-chars))
    (if (< padding 0) (setq padding 0))
    (dotimes (i (length str)) (%output-char (char str i) stream)
      (decf mincol))
    (if (> num-strs 1)
        (let ((pad (truncate padding (- num-strs 1))))
          (if (< pad minpad) (setq pad minpad))
          (dotimes (i pad) (%output-char padchar stream)(decf mincol))
          (justify-strings stream (cdr strings) (max mincol 0) colinc minpad
                           padchar)))))

(%set-format-dispatch-func
 #\<
 #'(lambda (stream args index
            atsign-modifier colon-modifier
            control &optional mincol colinc
                              minpad padchar)
     (declare
      (ignore colon-modifier
              atsign-modifier))
     (setq args (nthcdr index args))
     (let ((greater-than-index
             (find-matching-greater-than (car control)
                                         (cadr control)))
           string
           embedded-exprs)
       (if greater-than-index
           (setq string
                 (subseq (car control)
                         (cadr control)
                         greater-than-index))
           (error "Missing ~~> following ~~< in format string"))
       (setf (cadr control)
             (+ 2 greater-than-index))
       (setf embedded-exprs
             (%expr-list string))
       (unless mincol (setf mincol 0))
       (unless colinc (setf colinc 1))
       (unless minpad (setf minpad 0))
       (setq padchar (ensure-char padchar #\SPACE))
       (let ((new-strs 'nil))
         (dolist (x embedded-exprs)
           (let ((string-stream
                   (make-string-output-stream)))
             (incf index
                   (%format-list string-stream
                                 (car x)
                                 (nthcdr index
                                         args)))
             (push (get-output-stream-string string-stream)
                   new-strs)))
         (setq new-strs
               (nreverse new-strs))
         (if (= (length new-strs) 1)
             (push "" new-strs))
         (justify-strings stream
                          new-strs
                          mincol
                          colinc
                          minpad
                          padchar)
         index))))


;;;;
;;;;    Common Lisp FORMATTER macro.
;;;;
(defun formatter (string)
  #'(lambda (*standard-output* &rest arguments)
      (apply #'format-internal *standard-output* string arguments)))
