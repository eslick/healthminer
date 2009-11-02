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
		 (:file "classifiers")
		 (:file "bayes")
		 (:file "swirl")
		 (:file "naive-bayes")
		 (:file "lam-utils")
		 (:file "lam-message-db")
		 (:file "lam-patterns")
		 (:file "lam-lexical")
		 (:file "lam-crf")
		 (:file "lam-lda")
		 (:file "lam-listserv-mining")
		 (:file "lam-divisi"))
;;		 (:file "webtest"))
    :serial t
    :in-order-to ((load-op (compile-op :smart)))
    :depends-on (:stdutils :cl-json :hunchentoot :drakma :elephant :langutils :f-underscore
			   :trivial-shell :cxml :cxml-stp :cffi))

(defsystem #:smart-explorer    
    :description "SMART: Semantic Manipulation, Analytics, Reporting and Terms"
    :version "0.1"
    :author "Ian Eslick <eslick@media.mit.edu>"
    :licence "Media Lab Proprietary: non-profit application use"
    :components ((:file "explorer"))
    :serial t
    :in-order-to ((load-op (compile-op :smart-explorer)))
    :depends-on (:smart :weblocks))
