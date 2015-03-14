;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               package-fun.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Crate
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    new file for wrapping zpackage of xach and pjb into crate.
;;;;
;;;;AUTHORS
;;;;    <arvid> andy peterson <andy.arvid@gmail.com>,
;;;;LEGAL
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

(defclass crate ()
  ((name
    :initarg :name
    :reader crate-name)
   (packages
    :initarg :packages
    :reader crate-packages)
   (current-package-symbol
    :initform nil
    :accessor current-package-symbol)
   (keyword-package
    :initarg :keyword
    :accessor keyword-package)
   (common-lisp-package
    :initarg :keyword
    :accessor common-lisp-package)
   (common-lisp-user-package
    :initarg :keyword
    :accessor common-lisp-user-package)
  )
  (:default-initargs
   :name (gensym "CRATE")
   :packages (make-hash-table :test 'equal)))



(cl:defmacro with-crate ((var-or-crate
                          &optional (crate-if-var nil crate-if-var-supplied-p))
                         &body body)
  (let ((var (if crate-if-var-supplied-p var-or-crate (gensym)))
        (crate (if crate-if-var-supplied-p crate-if-var var-or-crate)))
   `(let ((,var ,crate))
      (let ((*crate* ,var))
        ,@body))))

(defun get-inferior-form (form)
  (cond ((consp form)
         (cons (get-inferior-form (car form))
               (get-inferior-form (cdr form))))
        ((symbolp form)
         (symbol-inferior form))
        (t form)))

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
         (export new-sym pck))
       new-sym))))

(defun promote-inferior-package (crate inferior-package)
  (let* ((pck-name (cl:package-name inferior-package)))
    (with-crate (crate)
      (let ((new-pck (crate:make-package pck-name)))
        (dolist (used (cl:package-use-list inferior-package) new-pck)
          (let ((upck (crate:find-package (cl:package-name used))))
            (when upck
              (crate:use-package upck new-pck))))))))

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

(defun ensure-current-package-symbol (crate)
  (when (common-lisp-package crate)
    (unless (current-package-symbol crate)
      (setf (current-package-symbol crate)
            (symbol-inferior (find-symbol "*PACKAGE*"
                                         (common-lisp-package crate)))))))

(defun crate-set-current-package (crate package)
  (ensure-current-package-symbol crate)
  (when (and (current-package-symbol crate)
             (cl:symbolp (common-lisp-package crate)))
    (set (current-package-symbol crate)
         package)))

(defun current-package* (crate)
  (when (current-package-symbol crate)
    (cl:symbol-value (current-package-symbol crate))))

(defun current-package ()
  (current-package* *crate*))

