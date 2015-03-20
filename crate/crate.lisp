;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               crate.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Crate
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Crate is a mutation of zpackage of xach and pjb for the clicl library.
;;;;    Crate allows for each clicl sandbox have its own package system but
;;;;    (as opposed to zpackage) it is not a complete replacement but a filter
;;;;    and redirection to underlying CL packages and symbols.
;;;;
;;;;AUTHORS
;;;;    <XACH> Zachary Beane <xach@xach.com>,
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;    <arvid> andy peterson <andy.arvid@gmail.com>,
;;;;LEGAL
;;;;    Copyright (c) 2012 Zachary Beane <xach@xach.com>, All Rights Reserved
;;;;    Copyright (c) 2012 Pascal J. Bourguignon <pjb@informatimago.com>, All Rights Reserved
;;;;    Copyright (c) 2015 andy peterson <andy.arvid@gmail.com>, All Rights Reserved
;;;;
;;;;    Redistribution and use in source and binary forms, with or without
;;;;    modification, are permitted provided that the following conditions
;;;;    are met:
;;;;
;;;;      * Redistributions of source code must retain the above copyright
;;;;        notice, this list of conditions and the following disclaimer.
;;;;
;;;;      * Redistributions in binary form must reproduce the above
;;;;        copyright notice, this list of conditions and the following
;;;;        disclaimer in the documentation and/or other materials
;;;;        provided with the distribution.
;;;;
;;;;    THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;;;    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;;    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;;    ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;;    DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;;    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;;    GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;;    INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;;    WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;;    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;;    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;**************************************************************************

(cl:in-package #:crate)


;;; conditions

(define-condition simple-error-mixin (condition)
  ((format-control   :initarg :format-control   :reader format-control
                     :initform "Simple error.")
   (format-arguments :initarg :format-arguments :reader format-arguments
                     :initform '()))
  (:report (lambda (condition stream)
             (format stream "~?"
                     (format-control condition)
                     (format-arguments condition)))))

(define-condition package-error (error)
  ((package :initarg :package :reader package-error-package))
  (:report (lambda (condition stream)
             (format stream "Package error with ~A" (package-error-package condition))))
  (:documentation "
The type package-error consists of error conditions related to operations on packages. 
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/e_pkg_er.htm>
"
                  ))


(define-condition simple-package-error (package-error simple-error-mixin)
  ())

(define-condition package-exists-error (simple-package-error)
  ()
  (:documentation "The error condition signaling that a package with the same name already exists."))

(define-condition package-does-not-exist-error (simple-package-error)
  ()
  (:documentation "The error condition signaling that no package with that name exists."))

(defgeneric symbol-conflict-existing-symbol (error)
  (:documentation "RETURN: The existing symbol in conflict."))

(defgeneric symbol-conflict-imported-symbol (error)
  (:documentation "RETURN: The imported symbol in conflict."))

(define-condition symbol-conflict-error (simple-package-error)
  ((existing-symbol :initarg :existing-symbol
                    :reader symbol-conflict-existing-symbol)
   (imported-symbol :initarg :imported-symbol
                    :reader symbol-conflict-imported-symbol))
  (:report (lambda (condition stream)
             (format stream "The would-be imported symbol ~S conflicts with the existing symbol ~S in the package ~S"
                     (symbol-conflict-imported-symbol condition)
                     (symbol-conflict-existing-symbol condition)
                     (package-name (package-error-package condition)))))
  (:documentation "The error condition signaling a symbol conflict."))

(define-condition symbol-inaccessible-error (simple-package-error)
  ((symbol :initarg :symbol :reader symbol-inaccessible-symbol))
  (:report (lambda (condition stream)
             (format stream "~S is not accessible in ~S"
                     (symbol-inaccessible-symbol condition)
                     (package-name (package-error-package condition))))))


;;; Variables
(defparameter *crate* nil)


;;; Symbols

(defclass <symbol> ()
  ((name
    :initarg :name
    :reader <symbol>-name)
   (pack
    :initarg :pack
    :reader <symbol>-<package>
    :accessor sym-pack)
   (symbol
    :initform nil
    :accessor <symbol>-symbol))
  (:default-initargs
   :pack nil)
  (:documentation " A symbol in a package of crate. "))





(defun make-<symbol> (sym-name)
  (make-instance '<symbol> :name (copy-seq sym-name)))

(defun crate::make-symbol (sym-name)
  (<symbol>-symbol (make-<symbol> sym-name)))





;;; Sym tables

(defgeneric make-sym-table ())
(defgeneric tget (sym-name table))
(defgeneric tput (sym table))
(defgeneric tremove (sym table))
(defgeneric tmember (sym table))
(defgeneric tmap-syms (fun table))
(defgeneric tmembers (table))

;;; Implementation of sym-tables

(defclass sym-table ()
  ((name-table
    :initarg :name-table
    :reader name-table))
  (:default-initargs
   :name-table (make-hash-table :test 'equal)))

(defmethod make-sym-table ()
  (make-instance 'sym-table))

(defmethod tget (sym-name table)
  (values (gethash sym-name (name-table table))))

(defmethod tmember (sym table)
  (let ((entry (tget (<symbol>-name sym) table)))
    (eq entry sym)))

(defmethod tput (sym table)
  (setf (gethash (<symbol>-name sym) (name-table table)) sym))

(defmethod tremove (sym table)
  (remhash (<symbol>-name sym) (name-table table)))

(defmethod tmap-syms (fun table)
  (maphash (lambda (sym-name sym)
             (declare (ignore sym-name))
             (funcall fun sym))
           (name-table table)))

(defmethod tmembers (table)
  (let ((members '()))
    (tmap-syms (lambda (sym)
                 (push sym members))
               table)
    members))



;;; packages 

(defclass <package> ()
  ((name
    :initarg :name
    :reader <package>-name
    :writer (setf name))
   (external-table
    :initarg :external-table
    :reader external-table)
   (present-table
    :initarg :present-table
    :reader present-table)
   (shadowing-table
    :initarg :shadowing-table
    :reader shadowing-table)
   (used-packs
    :initarg :used-packs
    :reader <package>-use-list
    :writer (setf used-packs))
   (used-by-packs
    :initarg :used-by-packs
    :reader <package>-used-by-list
    :writer (setf used-by-packs))
   (nicknames
    :initarg :nicknames
    :reader <package>-nicknames
    :writer (setf nicknames))
   (documentation
    :initarg :documentation
    :initform nil
    :accessor <package>-documentation)
   (package  
    :initform nil
    :reader <package>-package)
   (crate  
    :initarg :crate
    :reader <package>-crate))
  (:default-initargs
   :name (error "A package name is required")
   :external-table (make-sym-table)
   :present-table (make-sym-table)
   :shadowing-table (make-sym-table)
   :used-packs nil
   :used-by-packs nil)
  (:documentation " A package in a Crate. "))

(defmethod initialize-instance :after ((package <package>) &key)
  (setf (slot-value package 'package)
        (cl:make-package (gensym (<package>-name package))
                         :use nil :nicknames nil)))

;;; create the internal symbol for a <symbol>
(defmethod initialize-instance :after ((<symbol> <symbol>) &key)
  (setf (slot-value <symbol> 'symbol)
        (if (<symbol>-<package> <symbol>)
            (cl:intern (<symbol>-name <symbol>)
                       (<package>-package (<symbol>-<package> <symbol>)))
            (cl:make-symbol (<symbol>-name <symbol>)))))

(defmethod print-object ((pack <package>) stream)
  (if *print-readably*
      (error 'print-not-readable :object pack)
      (format stream "#<<~S> ~S>" 'package (<package>-name pack)))
  pack)


(defun accessiblep (sym pack)
  (let ((existing-sym (find-<symbol> (<symbol>-name sym) pack)))
    (eq existing-sym sym)))

(defun externalp (sym pack)
  (tmember sym (external-table pack)))

(defun shadowingp (sym pack)
  (tmember sym (shadowing-table pack)))

(defun presentp (sym pack)
  (tmember sym (present-table pack)))


(deftype crate::string-designator ()
  '(or string character symbol cl:symbol))


(defun normalize-string-designator (object)
  (typecase object
    (string     object)
    (character  (string object))
    (cl:symbol  (string object))
    (<symbol>   (<symbol>-name object))
    (otherwise  (error 'type-error
                       :datum object
                       :expected-type 'string-designator))))



(defun normalize-weak-designator-of-list-of-string-designator (object)
  (mapcan (lambda (nickname)
            (ensure-list (normalize-string-designator
                          nickname)))
          (ensure-list object)))

(defclass crate ()
  ((name
    :initarg :name
    :reader crate-name)
   (packages-external-name
    :initform (make-hash-table :test 'equal)
    :reader crate-packages-external-name)
   (packages-internal-name
    :initform (make-hash-table :test 'equal)
    :reader crate-packages-internal-name)
   (keyword-package
    :initarg :keyword
    :accessor keyword-package)
   (common-lisp-package
    :initarg :keyword
    :accessor common-lisp-package)
   (common-lisp-user-package
    :initarg :keyword
    :accessor common-lisp-user-package)
   (current-package-symbol
    :initform nil
    :accessor current-package-symbol))
  (:default-initargs
   :name (gensym "CRATE")))





(defun list-all-<packages> ()
  "
RETURN: A fresh list of all crate packages.
"
  (let ((packages '()))
    (maphash (lambda (k v) (declare (ignore k)) (pushnew v packages))
             (crate-packages-external-name *crate*))
    packages))

(defun list-all-packages ()
  "
RETURN: A fresh list of all registered packages in the current crate.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/f_list_a.htm>
"
  (let ((packages '()))
    (maphash (lambda (k v) (declare (ignore k))
               (pushnew (<package>-package v) packages))
             (crate-packages-internal-name *crate*))
    packages))


(defun package-to-<package> (package)
  (gethash (cl:package-name package)
           (crate-packages-internal-name *crate*)))




(deftype package-designator ()
  '(or package <package> string-designator))


(defun normalize-package-designator (object &key
                                            (if-package-does-not-exist :string)
                                            (if-package-exists :package)
                                            (if-not-a-package-designator :error))
  "
Normalize the given PACKAGE-DESIGNATOR.  Objects of type
PACKAGE-DESIGNATOR are either PACKAGE or objects of type
STRING-DESIGNATOR.

RETURN: either NIL, a STRING designating a non-existent package, or an
        existing <PACKAGE>.


IF-NOT-A-PACKAGE-DESIGNATOR The default is :ERROR.

    NIL                     If the OBJECT is not a PACKAGE-DESIGNATOR
                            then return NIL.

    :ERROR                  If the OBJECT is not a PACKAGE-DESIGNATOR
                            then signal a TYPE-ERROR.


If the object is a PACKAGE-DESIGNATOR, then the results depends on the
following parameters and whether the designated package exists or not.


IF-PACKAGE-DOES-NOT-EXIST   The default is :STRING

    NIL                     If the OBJECT designates a PACKAGE that
                            doesn't exist then return NIL.

    :STRING                 If the OBJECT designates a PACKAGE that
                            doesn't exist then (it would be a
                            STRING-DESIGNATOR) return the designated
                            STRING.

    :ERROR                  If the OBJECT designates a PACKAGE that
                            doesn't exist then signal a
                            PACKAGE-DOES-NOT-EXIST-ERROR.


IF-PACKAGE-EXISTS           The default is :PACKAGE

    :PACKAGE                If the OBJECT designates a PACKAGE that
                            does exist then return the designated
                            PACKAGE.

    :STRING                 If the OBJECT designates a PACKAGE that
                            does exist then return the designated
                            package name.

    :ERROR                  If the OBJECT designates a PACKAGE that
                            does exist then signal a
                            PACKAGE-EXISTS-ERROR.

"
  (check-type if-not-a-package-designator (member :error nil))
  (check-type if-package-does-not-exist   (member :error :string nil))
  (check-type if-package-exists           (member :error :string :package))

  (typecase object

    (string-designator
     (let* ((normalized  (normalize-string-designator object))
            (package     (find-<package> normalized)))
       (if package
           (normalize-package-designator package
                                         :if-package-exists if-package-exists)
           (case if-package-does-not-exist
             ((nil)         nil)
             ((:string)     normalized)
             ((:error)      (error
                             'package-does-not-exist-error
                             :package normalized
                             :format-control "There is no package named ~S"
                             :format-arguments (list normalized)))))))
    (<package>
     (case if-package-exists
       ((:package) object)
       ((:string)  (<package>-name object))
       ((:error)   (error
                    'package-exists-error
                    :package object
                    :format-control "There is already a package named ~S"
                    :format-arguments (list (<package>-name object))))))
    (package
     (if-let ((<p> (package-to-<package> object)))
       (normalize-package-designator <p>
                                     :if-package-exists if-package-exists)
       (case if-not-a-package-designator
         ((nil)     nil)
         ((:error)  (error 'type-error
                           :datum object
                           :expected-type 'package-designator)))))
    (otherwise
     (case if-not-a-package-designator
       ((nil)     nil)
       ((:error)  (error 'type-error
                         :datum object
                         :expected-type 'package-designator))))))

(defun find-<package> (pack-name)
  (etypecase pack-name
    (string-designator
     (values (gethash (normalize-string-designator pack-name)
                      (crate-packages-external-name *crate*))))
    (<package> pack-name)))

(defun crate::find-package (pack-name)
  (let ((name
          (etypecase pack-name
            (string-designator pack-name)
            (package (when-let (<p> (package-to-<package> pack-name))
                       (<package>-name <p>))))))
    (when-let ((p (and name (find-<package> name))))
      (<package>-package p))))


(defun check-import-conflict (sym pack)
  (let ((existing-sym (find-<symbol> (<symbol>-name sym) pack)))
    (if (and existing-sym (not (eq existing-sym sym)))
        (error 'symbol-conflict-error
               :package pack
               :format-control "Conflict: importing ~A into ~A conflicts with ~A"
               :format-arguments (list sym pack existing-sym)
               :existing-symbol existing-sym
               :imported-symbol sym)
        (values sym t))))


(defmacro zdo-external-symbols ((var pack) &body body)
  `(tmap-syms (lambda (,var)
                ,@body)
              (external-table ,pack)))

(defun check-inherit-conflict (used-pack using-pack)
  (zdo-external-symbols (inherited-sym used-pack)
    (let ((existing-sym (find-<symbol> (<symbol>-name inherited-sym)
                                       using-pack)))
      (when (and existing-sym
                 (not (eq inherited-sym existing-sym))
                 (not (shadowingp existing-sym using-pack)))
        (error "Conflict: Inheriting ~A from ~A conflicts with ~A in ~A"
               inherited-sym
               used-pack
               existing-sym
               using-pack)))))

(defun check-export-conflict (sym pack)
  (let ((sym-name (<symbol>-name sym)))
    (dolist (using-pack (<package>-used-by-list pack))
      (let ((existing-sym (find-<symbol> sym-name using-pack)))
        (when (and existing-sym
                   (not (member existing-sym
                                (<package>-shadowing-symbols using-pack))))
          (unless (eq existing-sym sym)
            (error "Conflict: exporting ~A conflicts with ~A in ~A"
                   sym existing-sym using-pack)))))))

(defun check-unintern-conflict (sym pack)
  (let ((sym-name (<symbol>-name sym))
        (first-existing-sym nil))
    (dolist (used-pack (<package>-use-list pack))
      (let ((existing-sym (find-<symbol> sym-name used-pack)))
        (when existing-sym
          (if first-existing-sym
              (unless (eq existing-sym first-existing-sym)
                (error "Conflict: uninterning ~A would lead to conflict ~
                      between ~A and ~A"
                       sym first-existing-sym existing-sym))
              (setf first-existing-sym existing-sym)))))))




(defun zimport-without-checks (sym pack)
  (tput sym (present-table pack))
  (unless (<symbol>-<package> sym)
    (setf (sym-pack sym) pack)
    (if (<symbol>-symbol sym)
        (cl:import (<symbol>-symbol sym) (<package>-package pack))
        (setf (slot-value sym 'symbol)
              (cl:intern (<symbol>-name sym) (<package>-package pack))))))

(defun zunintern-without-checks (sym pack)
  (tremove sym (external-table pack))
  (tremove sym (shadowing-table pack))
  (tremove sym (present-table pack))
  (when (eq (<symbol>-<package> sym) pack)
    (cl:unintern (<symbol>-symbol sym) (<package>-package pack))
    (setf (<symbol>-symbol sym) nil)
    (setf (sym-pack sym) nil)))

(defun check-new-names (pack-name nicknames &key renaming-package)
  (loop
    :with result = '()
    :for name :in (cons pack-name nicknames)
    :do (loop
          :for pack = (find-<package> name)
          :while (if renaming-package
                     (and pack (not (eq pack renaming-package)))
                     pack)
          :do (error 'package-exists-error
                     :package name
                     :format-control "A package named ~S already exists"
                     :format-arguments (list name))
          :finally (push name result))
    :finally (let ((result (nreverse result)))
               (return (values (car result) (cdr result))))))

(defun make-<package> (pack-name &key (nicknames '()) (use '()))
  (let ((pack-name (normalize-string-designator pack-name))
        (nicknames (normalize-weak-designator-of-list-of-string-designator
                    nicknames))
        (use       (mapcan (lambda (package-designator)
                             (list (normalize-package-designator
                                    package-designator
                                    :if-package-does-not-exist
                                    :error)))
                           use)))
    (multiple-value-setq (pack-name nicknames)
      (check-new-names pack-name nicknames))
    (let ((package (make-instance '<package>
                                  :name (copy-seq pack-name)
                                  :nicknames (mapcar (function copy-seq)
                                                     nicknames)
                                  :crate *crate*))
          (packs (crate-packages-external-name *crate*)))
      (dolist (upack use)
        (use-package upack package))
      (dolist (name (cons pack-name nicknames) package)
        (setf (gethash name packs) package))
      (setf (gethash (cl:package-name (<package>-package package))
                     (crate-packages-internal-name *crate*))
            package))))

(defun crate::make-package (pack-name &key (nicknames '()) (use '()))
  (<package>-package (make-<package> pack-name :nicknames nicknames :use use)))

(defun current-package* (crate)
  (when (current-package-symbol crate)
    (cl:symbol-value (current-package-symbol crate))))

(defun current-package ()
  (current-package* *crate*))

(defun use-<package> (packs &optional (using-pack (current-package)))
  (dolist (pack (ensure-list packs) t)
    (let* ((pack (normalize-package-designator
                  pack :if-package-does-not-exist :error))
           (using-pack (normalize-package-designator
                        using-pack :if-package-does-not-exist :error))
           (use-list   (<package>-use-list using-pack)))
      (unless (member pack use-list)
        (check-inherit-conflict pack using-pack)
        (setf (used-packs using-pack) (cons pack use-list))
        (setf (used-by-packs    pack)
              (cons using-pack (<package>-used-by-list pack)))))))

(defun crate::use-package (packs &optional (using-pack (current-package)))
  (use-<package> packs using-pack))


(defun unuse-<package> (packs &optional (using-pack (current-package)))
  (let ((using-pack (normalize-package-designator
                     using-pack :if-package-does-not-exist :error)))
    (dolist (pack (ensure-list packs) t)
      (let ((pack (normalize-package-designator
                   pack :if-package-does-not-exist :error)))
        (setf (used-packs using-pack)
              (remove pack (<package>-use-list using-pack)))
        (setf (used-by-packs pack)
              (remove using-pack (<package>-used-by-list pack)))))))

(defun crate::unuse-package (packs &optional (using-pack (current-package)))
  (unuse-<package> packs using-pack))



(defun find-<symbol> (sym-name &optional (pack (current-package)))
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error))
        sym)
    (cond ((setf sym (tget sym-name (external-table pack)))
           (values sym :external))
          ((setf sym (tget sym-name (shadowing-table pack)))
           (values sym :internal))
          ((setf sym (some (lambda (used-pack)
                             (tget sym-name (external-table used-pack)))
                           (<package>-use-list pack)))
           (values sym :inherited))
          ((setf sym (tget sym-name (present-table pack)))
           (values sym :internal))
          (t
           (values nil nil)))))

(defun crate::find-symbol (sym-name &optional (pack (current-package)))
  (multiple-value-bind (sym status) (find-<symbol> sym-name pack)
    (if sym
        (values (<symbol>-symbol sym) status)
        (values sym status))))




(defun crate:package-name (package)
  (<package>-name (normalize-package-designator
                   package
                   :if-package-does-not-exist :error)))
(defun crate:package-use-list (package)
  (mapcar #'<package>-package
          (<package>-use-list (normalize-package-designator
                               package
                               :if-package-does-not-exist :error))))

(defun crate:package-used-by-list (package)
  (mapcar #'<package>-package
          (<package>-used-by-list (normalize-package-designator
                                   package
                                   :if-package-does-not-exist :error))))
(defun crate:package-nicknames (package)
  (<package>-nicknames (normalize-package-designator
                        package
                        :if-package-does-not-exist :error)))

(defun <package>-shadowing-symbols (pack)
  (tmembers (shadowing-table pack)))

(defun crate:package-shadowing-symbols (package)
  (mapcar #'<symbol>-symbol
          (<package>-shadowing-symbols (normalize-package-designator
                                        package
                                        :if-package-does-not-exist :error))))



(defun delete-<package> (pack)
  (when (and pack (<package>-name pack))
    (dolist (used (<package>-used-by-list pack))
      (unuse-<package> pack used))
    (dolist (puse (<package>-use-list pack))
      (unuse-<package> puse pack))
    (tmap-syms (lambda (sym)
                 (when (eq (<symbol>-<package> sym) pack)
                   (zunintern-without-checks sym pack)))
               (present-table pack))
    (let ((packs (crate-packages-external-name *crate*)))
      (dolist (name (cons (<package>-name pack) (<package>-nicknames pack)))
        (remhash name packs)))
    (remhash (cl:package-name (<package>-package pack))
             (crate-packages-internal-name *crate*))
    (setf (name pack) nil)
    pack))


(defun crate::delete-package (pack)
  (when (and pack (or (not (cl:packagep pack))
                      (cl:package-name pack)))
    (let* ((pack (normalize-package-designator
                  pack :if-package-does-not-exist :error))
           (pkg (<package>-package pack)))
      (delete-<package> pack)
      (cl:delete-package pkg))))

(defun symbol-to-<symbol> (symbol)
  (if-let ((<p> (package-to-<package> (cl:symbol-package symbol))))
    (nth-value 0
               (find-<symbol> (cl:symbol-name symbol) <p>))
    (error "unknown symbol ~a" symbol)))


(defun <symbol>-import (symbols &optional (pack (current-package)))
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error)))
    (flet ((do-import (sym)
             (check-type sym <symbol>)
             (multiple-value-bind (sym good) (check-import-conflict sym pack)
               (when (and good (not (presentp sym pack)))
                 (zimport-without-checks sym pack)
                 (when (and (null (<symbol>-<package> sym))
                            (eql pack (keyword-package *crate*)))
                     #|(change-class sym 'keyword)|#
                     #|(make-constant sym sym)|#
                     (<symbol>-export sym pack))))))
      (mapc (function do-import) (ensure-list symbols)))
    t))

(defun crate:import (symbols &optional (pack (current-package)))
  (let ((symbols (mapcar #'symbol-to-<symbol>
                         (ensure-list symbols))))
   (<symbol>-import symbols pack)))

(defun <symbol>-intern (sym-name &optional (pack (current-package)))
  (check-type sym-name string)
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error)))
    (multiple-value-bind (sym status) (find-<symbol> sym-name pack)
      (if status
          (values sym status)
          (values (let ((sym (make-<symbol> sym-name)))
                    (<symbol>-import sym pack)
                    (when (eql pack (keyword-package *crate*))
                      #|(change-class sym 'keyword)|#
                      #|(make-constant sym sym)|#
                      (<symbol>-export sym pack))
                    sym)
                  nil)))))

(defun crate::intern (sym-name &optional (pack (current-package)))
  (multiple-value-bind (sym status) (<symbol>-intern sym-name pack)
    (if sym
        (values (<symbol>-symbol sym) status)
        (values sym status))))

(defun <symbol>-export (symbols &optional (pack (current-package)))
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error)))
    (flet ((do-export (sym)
             (check-type sym <symbol>)
             (unless (accessiblep sym pack)
               (error 'symbol-inaccessible-error :package pack :symbol sym))
             (check-export-conflict sym pack)
             (unless (presentp sym pack)
               (<symbol>-import sym pack))
             (tput sym (external-table pack))))
      (mapc (function do-export) (ensure-list symbols))
      t)))

(defun crate::export (symbols &optional (pack (current-package)))
  (let ((symbols (mapcar #'symbol-to-<symbol>
                         (ensure-list symbols))))
    (<symbol>-export symbols pack)))


(defun <symbol>-shadow (symbol-names &optional (pack (current-package)))
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error)))
    (flet ((do-shadow (sym-name)
             (let ((sym (tget sym-name (present-table pack))))
               (unless sym
                 (setf sym (make-<symbol> sym-name))
                 (zimport-without-checks sym pack))
               (tput sym (shadowing-table pack)))))
      (mapc (function do-shadow)
            (normalize-weak-designator-of-list-of-string-designator
             symbol-names)))
    t))

(defun crate::shadow (symbol-names &optional (pack (current-package)))
  (<symbol>-shadow symbol-names pack))

(defun <symbol>-shadowing-import (symbols &optional (pack (current-package)))
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error)))
    (flet ((do-shadowing-import (sym)
             (check-type sym <symbol>)
             (let ((sym-name (<symbol>-name sym)))
               (multiple-value-bind (existing-sym type)
                   (find-<symbol> sym-name pack)
                 (case type
                   ((nil :inherited)
                    (zimport-without-checks sym pack))
                   ((:external :internal)
                    (unless (eq existing-sym sym)
                      (zunintern-without-checks existing-sym pack)
                      (import sym pack))))
                 (tput sym (shadowing-table pack))))))
      (mapc (function do-shadowing-import) (ensure-list symbols))
      t)))

(defun crate::shadowing-import (symbols &optional (pack (current-package)))
  (let ((symbols (mapcar #'symbol-to-<symbol>
                         (ensure-list symbols))))
    (<symbol>-shadowing-import symbols pack)))

(defun <symbol>-unexport (symbols &optional (pack (current-package)))
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error)))
    (flet ((do-unexport (sym)
             (check-type sym symbol)
             (unless (accessiblep sym pack)
               (error 'symbol-inaccessible-error :package pack :symbol sym))
             (tremove sym (external-table pack))))
      (mapc (function do-unexport) (ensure-list symbols))
      t)))

(defun crate::unexport (symbols &optional (pack (current-package)))
  (let ((symbols (mapcar #'symbol-to-<symbol>
                         (ensure-list symbols))))
    (<symbol>-unexport symbols pack)))


(defun <symbol>-unintern (sym &optional (pack (current-package)))
  (let ((pack (normalize-package-designator
               pack :if-package-does-not-exist :error)))
    (when (accessiblep sym pack)
      (check-unintern-conflict sym pack)
      (zunintern-without-checks sym pack)
      t)))

(defun crate::unintern (sym &optional (pack (current-package)))
  (<symbol>-unintern (symbol-to-<symbol> sym) pack))


(defun find-all-<symbols> (name)
  (let ((name (normalize-string-designator name))
        (symbols '()))
    (dolist (pack (list-all-<packages>) (delete-duplicates symbols))
      (multiple-value-bind (sym found) (find-<symbol> name pack)
        (when found
          (push sym symbols))))))

(defun crate::find-all-symbols (name)
  (let ((name (normalize-string-designator name))
        (symbols '()))
    (dolist (pack (list-all-packages) (delete-duplicates symbols))
      (multiple-value-bind (sym found) (find-symbol name pack)
        (when found
          (push sym symbols))))))


(defun rename-<package> (package new-name &optional new-nicknames)
  (let ((package       (normalize-package-designator
                        package :if-package-does-not-exist :error))
        (new-name      (normalize-string-designator new-name))
        (new-nicknames (normalize-weak-designator-of-list-of-string-designator
                        new-nicknames))
        (packs (crate-packages-external-name *crate*)))
    (multiple-value-setq (new-name new-nicknames)
      (check-new-names new-name new-nicknames
                       :renaming-package package))
    ;; remove old names:
    (dolist (name (cons (<package>-name package) (<package>-nicknames package)))
      (remhash name packs))
    ;; set new names:
    (setf (name package) (copy-seq new-name)
          (nicknames package) (mapcar (function copy-seq) new-nicknames))
    (dolist (name (cons new-name new-nicknames) package)
      (setf (gethash name packs) package))))

(defun crate::rename-package (package new-name &optional new-nicknames)
  (let* ((package (rename-<package> package new-name new-nicknames))
         (old-iname (cl:package-name (<package>-package package)))
         (new-iname (gensym (<package>-name package))))
    (cl:rename-package (<package>-package package) new-iname)
    (remhash old-iname (crate-packages-internal-name *crate*))
    (setf (gethash new-iname (crate-packages-internal-name *crate*))
          package)))

(cl:defmacro with-crate ((var-or-crate
                          &optional (crate-if-var nil crate-if-var-supplied-p))
                         &body body)
  (let ((var (if crate-if-var-supplied-p var-or-crate (gensym)))
        (crate (if crate-if-var-supplied-p crate-if-var var-or-crate)))
   `(let ((,var ,crate))
      (let ((*crate* ,var))
        ,@body))))



(defun promote-inferior-package (crate inferior-package)
  (let* ((pck-name (cl:package-name inferior-package)))
    (with-crate (crate)
      (let ((new-pck (crate:make-package pck-name
                                         :nicknames
                                         (cl:package-nicknames inferior-package) )))
        (dolist (used (cl:package-use-list inferior-package) new-pck)
          (when-let ((upck (crate:find-package (cl:package-name used))))
            (crate:use-package upck new-pck)))))))

(defun inferior-exportedp (inferior-symbol)
  (eql (nth-value 1 (cl:find-symbol (cl:symbol-name inferior-symbol)
                                    (cl:symbol-package inferior-symbol)))
       :external))

(defun promote-inferior-symbol (crate inferior-symbol)
  (let* ((sym-name (cl:symbol-name inferior-symbol))
         (sym-package (cl:symbol-package inferior-symbol))
         (sym-package-name (cl:package-name sym-package)))
   (with-crate (crate)
     (let* ((pck (crate:find-package sym-package-name))
            (new-sym (crate:intern sym-name pck)))
       (when (inferior-exportedp inferior-symbol)
         (crate:export new-sym pck))
       new-sym))))


(defmethod initialize-instance :after ((crate crate) &key)
  (setf (common-lisp-package crate)
        (promote-inferior-package crate (cl:find-package :common-lisp)))
  (setf (keyword-package crate)
        (promote-inferior-package crate (cl:find-package :keyword)))
  (setf (common-lisp-user-package crate)
        (promote-inferior-package crate (cl:find-package :common-lisp-user)))
  (setf (current-package-symbol crate)
        (promote-inferior-symbol crate 'cl:*package*))
  (set (current-package-symbol crate)
       (common-lisp-user-package crate)))

#|


 (defun shadow-external-symbol (crate inferior-symbol
                               &optional alternative-inferior-package)
  (with-crate (crate)
    (let ((new-sym (promote-inferior-symbol crate inferior-symbol))
          (import-inferior-symbol
            (if alternative-inferior-package
                (cl:find-symbol (cl:symbol-name inferior-symbol)
                                alternative-inferior-package)
                inferior-symbol)))
      (cl:shadowing-import import-inferior-symbol
                           (package-inferior (symbol-package new-sym))))))

 
|#
