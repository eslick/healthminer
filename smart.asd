(defpackage #:smart.system
  (:use #:cl #:asdf))

(in-package #:smart.system)

(defsystem #:smart
    :description "SMART: Semantic Manipulation, Analytics, Reporting and Terms"
    :version "0.1"
    :author "Ian Eslick <eslick@media.mit.edu>"
    :licence "Media Lab Proprietary: non-profit application use"
    :components ((:file "package")
		 (:file "csv-import")
		 (:file "webtest"))
    :serial t
    :in-order-to ((load-op (compile-op :utils)))
    :depends-on (:utils :cl-json :hunchentoot :cl-who))