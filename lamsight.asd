;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; ASD File for Think Utilities Module

(defpackage #:lamsight.system
  (:use #:cl #:asdf))

(in-package #:lamsight.system)

(defsystem #:lamsight
    :description "LAMsight: Lisp data analysis server for LAMsight project"
    :version "0.1"
    :author "Ian Eslick <eslick@media.mit.edu>"
    :licence "Media Lab Proprietary: non-profit application use"
    :components ((:file "package")
		 (:file "server")
		 (:file "models")
		 (:file "query"))
    :serial t
    :in-order-to ((load-op (compile-op :utils)))
    :depends-on (:utils :cl-memcached :cl-json :clsql))