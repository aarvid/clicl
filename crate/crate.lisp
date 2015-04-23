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
;;;;    Copyright (c) 2015 Andrew Arvid Peterson <andy.arvid@gmail.com>, All Rights Reserved
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

(define-condition simple-type-error (simple-error-mixin type-error)
  ())


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

(define-condition package-symbol-locked-error (simple-package-error)
  ((symbol :initarg :sym-name :reader locked-symbol-name))
  (:report (lambda (condition stream)
             (format stream "Package ~S is locked for new symbols: ~S not interned"
                     (package-name (package-error-package condition))
                     (locked-symbol-name condition)))))

(define-condition package-define-locked-error (simple-package-error)
  ((symbol :initarg :sym-name :reader locked-symbol-name))
  (:report (lambda (condition stream)
             (format stream "Package ~S is locked for new defines: ~S not defined"
                     (package-name (package-error-package condition))
                     (locked-symbol-name condition)))))




;;; Variables
(defparameter *crate* nil)
(defparameter *print-crate* nil)


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

(defmethod print-object ((sym <symbol>) stream)
  (if *print-readably*
      (error 'print-not-readable :object sym)
      (if-let ((pkg (<symbol>-<package> sym)))
        (format stream "#<<~S> ~S::~S>" 'symbol (<package>-name pkg)
                (<symbol>-name sym))
        (format stream "#<<~S> #:~S>" 'symbol (<symbol>-name sym)))))





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
   (package  
    :initform nil
    :reader <package>-package)
   (crate  
    :initarg :crate
    :reader <package>-crate)
   (symbol-locked-p
    :initarg :symbol-locked-p
    :accessor <package>-symbol-locked-p)
   (define-locked-p
    :initarg :function-locked-p
    :accessor <package>-define-locked-p)
   )
  
  (:default-initargs
   :name (error "A package name is required")
   :external-table (make-sym-table)
   :present-table (make-sym-table)
   :shadowing-table (make-sym-table)
   :used-packs nil
   :used-by-packs nil
   :symbol-locked-p nil
   :define-locked-p nil)
  (:documentation " A package in a Crate. "))


;;; Note that the KEYWORD <package> is special and always refers
;;; to the underlying KEYWORD package of Common Lisp implementation.
(defmethod initialize-instance :after ((package <package>) &key)
  (setf (slot-value package 'package)
        (if (string= "KEYWORD" (<package>-name package))
            (cl:find-package "KEYWORD")
            (cl:make-package (gensym (<package>-name package))
                             :use nil :nicknames nil))))



(defmethod print-object ((pack <package>) stream)
  (if *print-readably*
      (error 'print-not-readable :object pack)
      (format stream "#<<~S> ~S>" 'package (<package>-name pack)))
  pack)



(defgeneric accessiblep (sym pack))
(defgeneric externalp (sym pack))
(defgeneric shadowingp (sym pack))
(defgeneric presentp (sym pack))

(defmethod accessiblep ((sym <symbol>) (pack <package>))
  (let ((existing-sym (find-<symbol> (<symbol>-name sym) pack)))
    (eq existing-sym sym)))

(defmethod externalp ((sym <symbol>) (pack <package>))
  (tmember sym (external-table pack)))

(defmethod shadowingp ((sym <symbol>) (pack <package>))
  (tmember sym (shadowing-table pack)))

(defmethod presentp ((sym <symbol>) (pack <package>))
  (tmember sym (present-table pack)))

(defmethod accessiblep ((sym symbol) (pack package))
  (when-let ((<p> (package-to-<package> pack))
             (<s> (symbol-to-<symbol> sym)))
    (accessiblep <s> <p>)))

(defmethod externalp ((sym symbol) (pack package))
  (when-let ((<p> (package-to-<package> pack))
             (<s> (symbol-to-<symbol> sym)))
    (externalp <s> <p>)))

(defmethod shadowingp ((sym symbol) (pack package))
  (when-let ((<p> (package-to-<package> pack))
             (<s> (symbol-to-<symbol> sym)))
    (shadowingp <s> <p>)))

(defmethod presentp ((sym symbol) (pack package))
  (when-let ((<p> (package-to-<package> pack))
             (<s> (symbol-to-<symbol> sym)))
    (presentp <s> <p>)))




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
            (ensure-list (normalize-string-designator nickname)))
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
   (symbols
    :initform (make-hash-table :test 'eq)
    :reader crate-symbols)
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


(defun link-symbol-<symbol> (sym <sym> &optional (crate *crate*) )
  (setf (slot-value <sym> 'symbol) sym)
  (when crate
    (setf (gethash sym (crate-symbols crate)) <sym>)))

(defun keyword-<package>-p (<pack>)
  (eql (<package>-package <pack>)
       (keyword-package (<package>-crate <pack>))))

(defun keyword-package-p (pack)
  (keyword-<package>-p (package-to-<package> pack)))


;;; create the internal symbol for a <symbol>
(defmethod initialize-instance :after ((<symbol> <symbol>) &key)
  (let* ((<pkg> (<symbol>-<package> <symbol>))
         (sym (if <pkg>
                  (cl:intern (<symbol>-name <symbol>)
                             (<package>-package <pkg>))
                  (cl:make-symbol (<symbol>-name <symbol>))))
         (crate (if <pkg>
                    (<package>-crate <pkg>)
                    *crate*)))
    (link-symbol-<symbol> sym <symbol> crate)))

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

    :<PACKAGE>              If the OBJECT designates a PACKAGE that
                            does exist then return the related
                            <PACKAGE>.

    :STRING                 If the OBJECT designates a PACKAGE that
                            does exist then return the designated
                            package name.

    :ERROR                  If the OBJECT designates a PACKAGE that
                            does exist then signal a
                            PACKAGE-EXISTS-ERROR.

"
  (check-type if-not-a-package-designator (member :error nil))
  (check-type if-package-does-not-exist   (member :error :string nil))
  (check-type if-package-exists           (member :error :string :package :<package>))

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
       ((:package) (<package>-package object))
       ((:<package>) object)       
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
    (if (and (not (keyword-<package>-p pack)) (<symbol>-symbol sym))
        (cl:import (<symbol>-symbol sym) (<package>-package pack))
        (setf (slot-value sym 'symbol)
              (cl:intern (<symbol>-name sym) (<package>-package pack))))
    (link-symbol-<symbol> (<symbol>-symbol sym) sym (<package>-crate pack))))

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
                                    :if-package-exists :<package>
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
                  pack :if-package-does-not-exist :error
                       :if-package-exists :<package>))
           (using-pack (normalize-package-designator
                        using-pack :if-package-exists :<package>
                                   :if-package-does-not-exist :error))
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
                     using-pack :if-package-exists :<package>
                                :if-package-does-not-exist :error)))
    (dolist (pack (ensure-list packs) t)
      (let ((pack (normalize-package-designator
                   pack :if-package-exists :<package>
                        :if-package-does-not-exist :error)))
        (setf (used-packs using-pack)
              (remove pack (<package>-use-list using-pack)))
        (setf (used-by-packs pack)
              (remove using-pack (<package>-used-by-list pack)))))))

(defun crate::unuse-package (packs &optional (using-pack (current-package)))
  (unuse-<package> packs using-pack))



(defun find-<symbol> (sym-name &optional (pack (current-package)))
  (let ((pack (normalize-package-designator
               pack :if-package-exists :<package>
                    :if-package-does-not-exist :error))
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
                   :if-package-exists :<package>
                   :if-package-does-not-exist :error)))

(defun crate:package-use-list (package)
  (mapcar #'<package>-package
          (<package>-use-list (normalize-package-designator
                               package :if-package-exists :<package>
                                       :if-package-does-not-exist :error))))

(defun crate:package-used-by-list (package)
  (mapcar #'<package>-package
          (<package>-used-by-list (normalize-package-designator
                                   package :if-package-exists :<package>
                                           :if-package-does-not-exist :error))))
(defun crate:package-nicknames (package)
  (<package>-nicknames (normalize-package-designator
                        package :if-package-exists :<package>
                                :if-package-does-not-exist :error)))

(defun <package>-shadowing-symbols (pack)
  (tmembers (shadowing-table pack)))

(defun crate:package-shadowing-symbols (package)
  (mapcar #'<symbol>-symbol
          (<package>-shadowing-symbols (normalize-package-designator
                                        package :if-package-exists :<package>
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
                  pack :if-package-exists :<package>
                       :if-package-does-not-exist :error))
           (pkg (<package>-package pack)))
      (delete-<package> pack)
      (cl:delete-package pkg))))

(defun symbol-to-<symbol> (symbol &optional (crate *crate*))
  (when crate
    (gethash symbol (crate-symbols crate))))


(defun <symbol>-import (symbols &optional (pack (current-package)))
  (let ((pack (normalize-package-designator
               pack :if-package-exists :<package>
                    :if-package-does-not-exist :error)))
    (flet ((do-import (sym)
             (check-type sym <symbol>)
             (multiple-value-bind (sym good) (check-import-conflict sym pack)
               (when (and good (not (presentp sym pack)))
                 (zimport-without-checks sym pack)
                 (when (and (null (<symbol>-<package> sym))
                            (keyword-<package>-p pack))

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
               pack :if-package-exists :<package>
                    :if-package-does-not-exist :error)))
    (multiple-value-bind (sym status) (find-<symbol> sym-name pack)
      (if status
          (values sym status)
          (progn
            (when (<package>-symbol-locked-p pack)
              (error 'package-symbol-locked-error
                     :package (<package>-package  pack)
                     :sym-name sym-name))
           (values (let ((sym (make-<symbol> sym-name)))
                     (<symbol>-import sym pack)
                     (when (keyword-<package>-p pack)
                       (<symbol>-export sym pack))
                     sym)
                   nil))))))

(defun crate::intern (sym-name &optional (pack (current-package)))
     (multiple-value-bind (sym status) (<symbol>-intern sym-name pack)
       (if sym
           (values (<symbol>-symbol sym) status)
           (values sym status))))

(defun <symbol>-export (symbols &optional (pack (current-package)))
  (let ((pack (normalize-package-designator
               pack :if-package-exists :<package>
                    :if-package-does-not-exist :error)))
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
               pack :if-package-exists :<package>
                    :if-package-does-not-exist :error)))
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
               pack :if-package-exists :<package>
                    :if-package-does-not-exist :error)))
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
               pack :if-package-exists :<package>
                    :if-package-does-not-exist :error)))
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
               pack :if-package-exists :<package>
                    :if-package-does-not-exist :error)))
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
                        package :if-package-exists :<package>
                                :if-package-does-not-exist :error))
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
  (let* ((package (normalize-package-designator
                   package :if-package-exists :package
                           :if-package-does-not-exist :error))
         (old-name (package-name package))
         (<package> (rename-<package> package new-name new-nicknames)))
    (unless (string= old-name (<package>-name <package>))
      (let ((old-iname (cl:package-name package))
            (new-iname (gensym (<package>-name package))))
        (cl:rename-package (<package>-package package) new-iname)
        (remhash old-iname (crate-packages-internal-name *crate*))
        (setf (gethash (cl:package-name package)
                       (crate-packages-internal-name *crate*))
              package)))))

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


(defun package-symbol-locked-p (package)
  (<package>-symbol-locked-p
   (normalize-package-designator package :if-package-exists :<package>
                                         :if-package-does-not-exist :error)))

(defun package-define-locked-p (package)
  (<package>-define-locked-p
   (normalize-package-designator package :if-package-exists :<package>
                                         :if-package-does-not-exist :error)))


(defsetf package-symbol-locked-p (package) (val)
  `(setf (<package>-symbol-locked-p
           (normalize-package-designator ,package :if-package-exists :<package>
                                         :if-package-does-not-exist :error))
         ,val))


(defsetf package-define-locked-p (package) (val)
  `(setf (<package>-define-locked-p
           (normalize-package-designator ,package :if-package-exists :<package>
                                         :if-package-does-not-exist :error))
         ,val))


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
       (common-lisp-user-package crate))
  
  (with-crate (crate)
    (setf (package-define-locked-p (keyword-package crate)) t)
    (setf (package-symbol-locked-p (common-lisp-package crate)) t)
    (setf (package-define-locked-p (common-lisp-package crate)) t)))

(defun shadow-external-symbol (crate inferior-symbol
                               &optional alternative-inferior-package)
  (with-crate (crate)
    (let* ((new-sym (promote-inferior-symbol crate inferior-symbol))
           (<new-sym> (symbol-to-<symbol> new-sym))
           (superior-package (cl:symbol-package new-sym))
           (import-inferior-symbol
             (if alternative-inferior-package
                 (cl:find-symbol (cl:symbol-name inferior-symbol)
                                 alternative-inferior-package)
                 inferior-symbol)))
      (cl:shadowing-import import-inferior-symbol superior-package)
      (link-symbol-<symbol> import-inferior-symbol <new-sym> crate)
      inferior-symbol)))

(defun get-crate-symbol (crate inferior-symbol)
  (with-crate (crate)
    (find-symbol (cl:symbol-name inferior-symbol)
                 (cl:package-name (cl:symbol-package inferior-symbol)))))


(defmacro crate::in-package (name)
  "
DO:     Sets the current *package* to the package designated by NAME.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/m_in_pkg.htm>
"
  (let ((name (normalize-string-designator name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((new-package (normalize-package-designator
                           ,name :if-package-exists :package
                                 :if-package-does-not-exist
                           :error)))
         (when new-package
           (set (current-package-symbol *crate*) new-package))))))


(defun check-disjoints (shadows shadowing-import-froms import-froms
                        interns exports)
  (loop
    :for sets :in (list (append (list shadows interns)
                                (mapcar (function second) import-froms)
                                (mapcar (function second) shadowing-import-froms))
                        (list interns exports))
    :do (loop
          :for lefts :on sets
          :for left = (first lefts)
          :while (rest lefts)
          :do (loop
                :for rights :on (rest lefts)
                :for right = (first rights)
                :for inter = (intersection left right :test (function string=))
                :do (when inter
                      (flet ((set-name (set)
                               (let ((name (cdr (assoc set (list (cons shadows :shadow)
                                                                 (cons interns :intern)
                                                                 (cons exports :export))))))
                                 (or name
                                     (let ((name (first (find set shadowing-import-froms :key (function rest)))))
                                       (when name (list :shadowing-import-from name)))
                                     (let ((name (first (find set import-froms :key (function rest)))))
                                       (when name (list :import-from name)))))))
                        (error 'simple-program-error
                               :format-control "Symbol names in common between ~S and ~S: ~S"
                               :format-arguments (list (set-name left) (set-name right) inter)))))))
  nil)

(defun %define-package (name shadows shadowing-imports
                        uses imports interns exports
                        documentation nicknames)
  (flet ((find-symbols (import-package names option)
           (mapcan (lambda (name)
                     (multiple-value-bind (symbol status)
                         (find-symbol name import-package)
                       (if (null status)
                           (progn
                             (cerror (format nil "Ignore (~S  ~~*~~S ~~*~~S)" option)
                                     'symbol-does-not-exist-error
                                     :package import-package
                                     :symbol-name name)
                             '())
                           (list symbol))))
                   names)))
    (let ((package (find-package name)))
      (if package
          (let ((unuse-list (set-difference
                             (mapcar (lambda (np) (if (stringp np) np (package-name np)))
                                     (package-use-list package))
                             uses :test (function string=))))
            (rename-package package name nicknames)
            (when unuse-list
             (unuse-package unuse-list package)))
          (setf package (make-package name :nicknames nicknames :use '())))
      (setf (documentation package t) documentation)
      ;; 1. :shadow and :shadowing-import-from.
      (shadow shadows package)
      (loop
        :for (import-package symbols) :in shadowing-imports
        :do (shadowing-import (find-symbols import-package symbols
                                            :shadowing-import-from)
                              package))
      ;; 2. :use.
      (dolist (upack uses)
        (use-package upack package))
      ;; 3. :import-from and :intern.
      (loop
        :for (import-package symbols) :in imports
        :do (import (find-symbols import-package symbols
                                  :import-from)
                    package))
      (dolist (name interns)
        (intern name package))
      ;; 4. :export.
      (export (mapcar (lambda (name) (intern name package)) exports) package)
      package)))



(defmacro crate::defpackage (defined-package-name &rest options)
  "
DO:     Define a new package.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/m_defpkg.htm>
"
  ;; option::= (:nicknames nickname*)* |  
  ;;           (:documentation string) |  
  ;;           (:use package-name*)* |  
  ;;           (:shadow {symbol-name}*)* |  
  ;;           (:shadowing-import-from package-name {symbol-name}*)* |  
  ;;           (:import-from package-name {symbol-name}*)* |  
  ;;           (:export {symbol-name}*)* |  
  ;;           (:intern {symbol-name}*)* |  
  ;;           (:size integer)
  (dolist (option options)
    (unless (typep option 'list)
      (error 'simple-type-error
             :datum option
             :expected-type 'list
             :format-control "This implementation doesn't support any non-standard option such as ~S"
             :format-arguments (list option)))
    (unless (typep (car option) '(member :nicknames :documentation :use
                                  :shadow :shadowing-import-from
                                  :import-from :export :intern :size))
      (error 'simple-type-error
             :datum (car option)
             :expected-type '(member :nicknames :documentation :use
                              :shadow :shadowing-import-from
                              :import-from :export :intern :size)
             :format-control "This implementation doesn't support any non-standard option such as ~S"
             :format-arguments (list option))))
  (dolist (key '(:documentation :size))
    (unless (<= (count key options :key (function first)) 1)
      (cerror "Ignore all but the first" 'simple-program-error
              :format-control "Too many ~S options given: ~S"
              :format-arguments (list key (remove key options :test-not (function eql) :key (function first))))))
  (labels ((extract-strings (key)
             (delete-duplicates
              (normalize-weak-designator-of-list-of-string-designator
               (reduce (function append)
                       (mapcar (function rest)
                               (remove key options
                                       :key (function first)
                                       :test-not (function eql)))))))
           (extract-packages (key)
             (delete-duplicates
              (mapcan (lambda (package)
                        (list (normalize-package-designator
                               package
                               :if-package-does-not-exist :error
                               :if-package-exists :string)))
                      (reduce (function append)
                              (mapcar (function rest)
                                      (remove key options
                                              :key (function first)
                                              :test-not (function eql)))))))           
           (extract-from (key)
             (let ((table (make-hash-table))
                   (result '()))
               (dolist (entry (remove key options
                                      :key (function first)
                                      :test-not (function eql)))
                 (let ((entry (rest entry)))
                   (appendf (gethash (normalize-package-designator
                                      (first entry)
                                      :if-package-exists :package
                                      :if-package-does-not-exist :error)
                                     table)
                            (normalize-weak-designator-of-list-of-string-designator
                             (rest entry)))))
               ;; should do the same as in classify-per-package below.
               (maphash (lambda (k v) (push (list k v) result))
                        table)
               result))
           (check-string (object)
             (check-type object string)
             object)
           (extract-one-string (key)
             (let ((entries (remove key options
                                    :key (function first)
                                    :test-not (function eql))))
               (let ((entry (first entries)))
                 (when (rest entry)
                   (assert (null (cddr entry))
                           () "Invalid :DOCUMENTATION option: it should contain only one string.")
                   (check-string (second entry)))))))
    (let* ((shadows           (extract-strings    :shadow))
           (shadowing-imports (extract-from       :shadowing-import-from))
           (import-froms      (extract-from       :import-from))
           (interns           (extract-strings    :intern))
           (exports           (extract-strings    :export)))
      (check-disjoints shadows shadowing-imports import-froms interns exports)
      `(eval-when (:execute :compile-toplevel :load-toplevel)
         (%define-package ',(normalize-string-designator defined-package-name)
                          ',shadows
                          ',shadowing-imports
                          ',(extract-packages   :use)
                          ',import-froms
                          ',interns
                          ',exports
                          ',(extract-one-string :documentation)
                          ',(extract-strings    :nicknames))))))



(defun table-map-symbols (symbol-fun table-fun package)
  (when-let ((<p> (package-to-<package> package)))
    (tmap-syms (compose symbol-fun #'<symbol>-symbol)
               (funcall table-fun <p>))))

(defun make-package-iterator (packages symbol-types)
  (let ((packages (mapcan (lambda (package-designator)
                            (list (normalize-package-designator
                                   package-designator
                                   :if-package-exists :package
                                   :if-package-does-not-exist :error)))
                          (ensure-list packages)))
        (package  nil)
        (stypes   nil)
        (stype    nil)
        (symbols  '()))
    (labels ((iterator ()
               (cond
                 (symbols    (let ((sym (pop symbols)))
                               (values t
                                       sym
                                       (cond
                                         ((externalp sym package) :external)
                                         ((eq stype :inherited)   stype)
                                         (t                       :internal))
                                       package)))
                 (stypes     (setf stype (pop stypes))
                             (ecase stype
                               ((:internal)
                                (table-map-symbols (lambda (sym)
                                                     (unless (externalp sym package)
                                                       (push sym symbols)))
                                                   #'present-table
                                                   package))
                               ((:external)
                                (table-map-symbols (lambda (sym)
                                                     (push sym symbols))
                                                   #'external-table
                                                   package))
                               ((:inherited)
                                (dolist (pack (package-use-list package))
                                  (table-map-symbols
                                   (lambda (sym)
                                     (let ((shadow (find-symbol (symbol-name sym)
                                                                package)))
                                       (unless (and shadow
                                                    (shadowingp shadow package)
                                                    (not (eq sym shadow)))
                                         (push sym symbols))))
                                   #'external-table
                                   (find-package pack))))
                               ((:present)
                                (table-map-symbols (lambda (sym) (push sym symbols))
                                                   #'present-table
                                                   package))
                               ((:shadowing)
                                (table-map-symbols (lambda (sym) (push sym symbols))
                                                   #'shadowing-table
                                                   package)))
                             (iterator))
                 (packages   (setf package (pop packages)
                                   stypes  symbol-types)
                             (iterator))
                 (t          nil))))
      (function iterator))))


(defmacro crate:with-package-iterator ((name package-list-form
                                        &rest symbol-types)
                                       &body declarations-body)
  "
DO:     Within the lexical scope of the body forms, the name is
        defined via macrolet such that successive invocations of
        (name) will return the symbols, one by one, from the packages
        in package-list.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/m_w_pkg_.htm>
"
  (flet ((valid-symbol-type-p (object)
           (member object '(:internal :external :inherited
                            ;; extensions:
                            :present :shadowing))))
    (cond
      ((null symbol-types) (error 'simple-program-error
                                  :format-control "Missing at least one symbol-type"))
      ((every (function valid-symbol-type-p) symbol-types))
      (t (error 'simple-program-error
                :format-control "Invalid symbol-type: ~S"
                :format-arguments (list (find-if-not (function valid-symbol-type-p)
                                                     symbol-types))))))
  (let ((viterator (gensym "ITERATOR")))
    `(let ((,viterator (make-package-iterator ,package-list-form ',symbol-types)))
       (macrolet ((,name () '(funcall ,viterator)))
         ,@declarations-body))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun declarations (body)
    (loop
      :for item :in body
      :while (and (listp item) (eql 'declare (car item)))
      :collect item))

  (defun body (body)
    (loop
      :for items :on body
      :for item = (car items)
      :while (and (listp item) (eql 'declare (car item)))
      :finally (return items)))


  (defun generate-do-symbols-loop (var package result-form body symbol-types)
    (let ((iter   (gensym "ITERATOR"))
          (got-it (gensym "GOT-IT"))
          (symbol (gensym "SYMBOL"))
          (vpack  (gensym "PACKAGE")))
      `(let ((,vpack (or ,package *package*)))
         (with-package-iterator (,iter ,vpack ,@symbol-types)
           (let (,var)
             ,@(declarations body)
             (loop
               (multiple-value-bind (,got-it ,symbol) (,iter)
                 (if ,got-it
                     (tagbody
                        (setf ,var ,symbol)
                        ,@(body body))
                     (progn
                       (setf ,var nil)
                       (return ,result-form))))))))))

  ) ; end of eval-when

(defmacro crate::do-symbols ((var &optional package result-form) &body body)
  "
DO:     Iterate over all the symbols of the package.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/m_do_sym.htm>
"
  (generate-do-symbols-loop var package result-form body
                            '(:internal :external :inherited)))


(defmacro crate::do-external-symbols ((var &optional package result-form)
                                     &body body)
  "
DO:     Iterate over all the external symbols of the package.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/m_do_sym.htm>
"
  (generate-do-symbols-loop var package result-form body '(:external)))


(defmacro crate::do-all-symbols ((var &optional result-form) &body body)
  "
DO:     Iterate over all the symbols of all the packages.
URL:    <http://www.lispworks.com/documentation/HyperSpec/Body/m_do_sym.htm>
"
  (generate-do-symbols-loop var '(list-all-packages) result-form body
                            '(:internal :external :inherited)))




(defun check-can-define-symbol (symbol)
  (if-let (<sym> (symbol-to-<symbol> symbol))
    (if-let (<pkg> (<symbol>-<package> <sym>))
      (when (<package>-define-locked-p <pkg>)
        (error 'package-define-locked-error
            :package (<package>-package <pkg>)
            :sym-name (cl:symbol-name symbol)))
      (error "Internal Error: <symbol> ~s lacks package" <sym> ))
    (error "Internal Error: symbol ~s is missing" symbol)))





(defun crate:symbol-package (symbol)
  (when-let* ((<sym> (symbol-to-<symbol> symbol))
              (<pkg> (<symbol>-<package> <sym>)))
    (<package>-package <pkg>)))

(defun crate:packagep (object)
  (if (packagep object)
      (if (package-to-<package> object)
          t)))


(cl:defmacro with-crate-locks-disabled ((crate package) &body body)
  (with-gensyms (symp defp) 
   `(let ((*crate* ,crate)
          (,symp (package-symbol-locked-p ,package))
          (,defp (package-define-locked-p ,package)))
      (setf (package-symbol-locked-p ,package) nil)
      (setf (package-define-locked-p ,package) nil)
      (unwind-protect
           (progn ,@body)
        (progn
          (setf (package-symbol-locked-p ,package) ,symp)
          (setf (package-define-locked-p ,package) ,defp))))))



(defun constituentp (ch first-character-p &optional (readtable *readtable*))
  (multiple-value-bind (macro-character-p non-terminating-p) (get-macro-character ch readtable)
    (or (not macro-character-p)
        (and (not first-character-p)
             non-terminating-p))))
;;??
(defun specialp (ch &optional (readtable *readtable*))
  (declare (ignore readtable))
  (find ch #(#\Space #\: #\| #\\
             #\Newline #\Tab #\Linefeed #\Return #\Page)))

;;??
(defun parses-as-a-number-p (string &key (start 0) (end nil) (base *read-base*))
  ;; integer  ::= [sign] digit+
  ;; integer  ::= [sign] decimal-digit+ decimal-point 
  ;; ratio    ::= [sign] {decimal-digit}+ '/' {decimal-digit}+
  ;; float    ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ exponent
  ;; float    ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ 
  ;; float    ::= [sign] {decimal-digit}+ exponent
  ;; float    ::= [sign] {decimal-digit}+ decimal-point {decimal-digit}* exponent
  ;; exponent ::=  exponent-marker [sign] {digit}+
  ;; We may ignore ratio starting with #\# since that's excluded by constituentp.
  ;; ratio    ::= [#b|#o|#x|#{decimal-digit}+r] [sign] digit+ '/' digit+
  (loop
    :with end =  (or end (length string))
    :with i = start
    :with state = :opt-sign
    :for ch = (and (< i end) (aref string i))
    :while (< i end)
    :do (ecase state
          (:opt-sign (case ch ((#\+ #\-) (incf i)))
                     (setf state :unknown0))
          (:unknown0  (if (<= base 10)
                          (cond
                            ((digit-char-p ch base) (incf i) (setf state :unknown1))
                            ((digit-char-p ch 10)   (incf i) (setf state :decimal))
                            (t (case ch
                                 ((#\.) (incf i) (setf state :float0))
                                 (otherwise (return nil)))))
                          (cond
                            ((digit-char-p ch 10)   (incf i) (setf state :unknown1))
                            ((digit-char-p ch base) (incf i) (setf state :integer))
                            (t (case ch
                                 ((#\.) (incf i) (setf state :float0))
                                 (otherwise (return nil)))))))
          (:unknown1  (if (<= base 10)
                          (cond
                            ((digit-char-p ch base) (incf i) (setf state :unknown1))
                            ((digit-char-p ch 10)   (incf i) (setf state :decimal))
                            (t (case ch
                                 ((#\/) (incf i) (setf state :ratio0))
                                 ((#\.) (incf i) (setf state :dot))
                                 ((#\D #\d #\E #\e #\F #\f #\L #\l #\S #\s)
                                  (incf i) (setf state :float-expo/opt-sign))
                                 (otherwise (return nil)))))
                          (cond
                            ((digit-char-p ch 10)   (incf i) (setf state :unknown1))
                            ((digit-char-p ch base) (incf i) (setf state :integer))
                            (t (case ch
                                 ((#\/) (incf i) (setf state :ratio0))
                                 ((#\.) (incf i) (setf state :dot))
                                 ((#\D #\d #\E #\e #\F #\f #\L #\l #\S #\s)
                                  (incf i) (setf state :float-expo/opt-sign))
                                 (otherwise (return nil)))))))
          (:integer   (if (digit-char-p ch base)
                          (incf i)
                          (return nil)))
          (:decimal   (if (digit-char-p ch 10)
                          (incf i)
                          (case ch
                            ((#\/) (incf i) (setf state :ratio0))
                            ((#\.) (incf i) (setf state :dot))
                            ((#\D #\d #\E #\e #\F #\f #\L #\l #\S #\s)
                             (incf i) (setf state :float-expo/opt-sign))
                            (otherwise (return nil)))))
          (:dot      (if (digit-char-p ch 10)
                         (progn (incf i) (setf state :float))
                         (case ch
                           ((#\D #\d #\E #\e #\F #\f #\L #\l #\S #\s)
                            (incf i) (setf state :float-expo/opt-sign))
                           (otherwise (return nil)))))
          (:ratio0   (if (digit-char-p ch 10)
                         (progn (incf i) (setf state :ratio))
                         (return nil)))
          (:ratio    (if (digit-char-p ch 10)
                         (incf i)
                         (return nil)))
          (:float0   (if (digit-char-p ch 10)
                         (progn (incf i) (setf state :float))
                         (return nil)))
          (:float    (if (digit-char-p ch 10)
                         (incf i)
                         (case ch
                           ((#\D #\d #\E #\e #\F #\f #\L #\l #\S #\s)
                            (incf i) (setf state :float-expo/opt-sign))
                           (otherwise (return nil)))))
          (:float-expo/opt-sign (case ch ((#\+ #\-) (incf i)))
                                (setf state :float-expo0))
          (:float-expo0 (if (digit-char-p ch 10)
                            (progn (incf i) (setf state :float-expo))
                            (return nil)))
          (:float-expo  (if (digit-char-p ch 10)
                            (incf i)
                            (return nil))))
    :finally (return (case state
                       ((:unknown1 :integer :dot :ratio :float :float-expo) t)
                       (otherwise nil)))))


(defun needs-escape-p (symbol-name)
  "Whether the symbol name needs to be escaped."
  (cond
    ((string= "" symbol-name) t)
    ((or *print-readably* *print-escape*)
     (or (notevery (let ((first-character-p t))
                     (lambda (ch)
                       (prog1 (and (not (specialp ch))
                                   (constituentp ch first-character-p))
                         (setf first-character-p nil))))
                   symbol-name)
         ;; Parses as a number integer, decimal, ratio or float.
         (parses-as-a-number-p symbol-name :base *print-base*)))
    (t
     nil)))

(defun mixed-case-p (string)
  "Whether the string contains both lower case and upper case letters."
  (and (some (lambda (ch) (and (alpha-char-p ch) (upper-case-p ch))) string)
       (some (lambda (ch) (and (alpha-char-p ch) (lower-case-p ch))) string)))

(defun prepare-symbol-name (sname)
  (cond
    ((needs-escape-p sname)
     (with-output-to-string (*standard-output*)
       (loop
         :for ch :across sname
         :initially (princ "|")
         :do (if (char= #\| ch) (princ "\\|") (princ ch))
         :finally (princ "|"))))
    (t
     (let ((transform 
            (if *print-escape*
                (ecase (readtable-case *readtable*)
                  (:upcase     (lambda (ch)
                                 (if (both-case-p ch)
                                     (if (lower-case-p ch)
                                         (format nil "\\~C" ch)
                                         ch)
                                     ch)))
                  (:downcase   (lambda (ch)
                                 (if (both-case-p ch)
                                     (if (upper-case-p ch)
                                         (format nil "\\~C" ch)
                                         ch))))
                  (:preserve   (function identity))
                  (:invert     (function identity)))   
                (ecase (readtable-case *readtable*)
                  (:upcase     (let ((start-word t))
                                 (lambda (ch)
                                   (prog1 (if (both-case-p ch)
                                              (if (upper-case-p ch)
                                                  (ecase *print-case*
                                                    (:upcase     ch)
                                                    (:downcase   (char-downcase ch))
                                                    (:capitalize (if start-word
                                                                     (char-upcase ch)
                                                                     (char-downcase ch))))
                                                  ch)
                                              ch)
                                     (if (alphanumericp ch)
                                         (setf start-word nil)
                                         (setf start-word t))))))
                  (:downcase   (let ((start-word t))
                                 (lambda (ch)
                                   (prog1 (if (both-case-p ch)
                                              (if (lower-case-p ch)
                                                  (ecase *print-case*
                                                    (:upcase     (char-upcase ch))
                                                    (:downcase   ch)
                                                    (:capitalize (if start-word
                                                                     (char-upcase ch)
                                                                     (char-downcase ch))))
                                                  ch)
                                              ch)
                                     (if (alphanumericp ch)
                                         (setf start-word nil)
                                         (setf start-word t))))))
                  (:preserve   (function identity))
                  (:invert     (if (mixed-case-p sname)
                                   (function identity)
                                   (lambda (ch)
                                     (cond
                                       ((not (both-case-p ch)) ch)
                                       ((upper-case-p ch)      (char-downcase ch))
                                       ((lower-case-p ch)      (char-upcase ch))
                                       (t                      ch)))))))))
       (with-output-to-string (*standard-output*)
         (loop
           :for ch :across sname
           :do (princ (funcall transform ch))))))))


#|
 note that this does not work because in SBCL printing of symbols is hard
 coded and does not use print-object.
 (defmethod print-object :around ((sym symbol) stream)
  (let ((str (with-output-to-string (s) (call-next-method sym s) )))
    (if *print-crate*
        (let ((pack (crate:symbol-package sym))
              (sym-name (cl:symbol-name sym)))
          (cond ((null pack)
                 (format stream "~:[~;#:~]~A"
                         (or *print-readably* (and *print-escape* *print-gensym*))
                         (prepare-symbol-name sym-name)))
                ((eql pack (keyword-package *crate*))
                 (format stream ":~A"
                         (prepare-symbol-name sym-name)))
                ((or (eq pack (current-package))
                     (eq sym (crate:find-symbol sym-name
                                                (current-package))))
                 (format stream "~A" (prepare-symbol-name sym-name)))
                (t
                 (format stream "~A~:[::~;:~]~A"
                         (prepare-symbol-name (crate:package-name pack))
                         (externalp sym pack)
                         (prepare-symbol-name sym-name)))))
        (format stream "~A" str))
    sym))
|#

(defun test-print-symbol (sym stream)
  (let ((pack (crate:symbol-package sym))
        (sym-name (cl:symbol-name sym)))
    (cond ((null pack)
           (format stream "~:[~;#:~]~A"
                   (or *print-readably* (and *print-escape* *print-gensym*))
                   (prepare-symbol-name sym-name)))
          ((eql pack (keyword-package *crate*))
           (format stream ":~A"
                   (prepare-symbol-name sym-name)))
          ((or (eq pack (current-package))
               (eq sym (crate:find-symbol sym-name
                                          (current-package))))
           (format stream "~A" (prepare-symbol-name sym-name)))
          (t
           (format stream "~A~:[::~;:~]~A"
                   (prepare-symbol-name (crate:package-name pack))
                   (externalp sym pack)
                   (prepare-symbol-name sym-name))))
    sym))

#|(defmethod print-object :around ((p package) stream )
  (let ((str (with-output-to-string (s) (call-next-method p s) )))
    (if *print-crate*
        (format stream "#<PACKAGE ~A>" (crate:package-name p))
        (format stream "~A" str ))
    p))|#
