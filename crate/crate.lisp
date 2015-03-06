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
   (current-package
    :initarg :current
    :accessor current-package)
   (keyword-package
    :initarg :keyword
    :accessor keyword-package)
  )
  (:default-initargs
   :name (gensym "PT")
   :packages (make-hash-table :test 'equal)))


(defmacro with-crate ((crate) &body body)
  `(let ((*package* (current-package ,crate))
         (*packs* (packages ,crate))
         (*keyword-package* (keyword-package ,crate)))
     ,@body))

(defun get-genuine-form (form)
  (cond ((consp form)
         (cons (get-genuine-form (car form))
               (get-genuine-form (cdr form))))
        ((symbolp form)
         (symbol-genuine form))
        (t form)))
