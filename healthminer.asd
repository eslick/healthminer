(defpackage #:healthminer.system
  (:use #:cl #:asdf))

(in-package #:healthminer.system)

(defsystem #:healthminer
    :description "HealthMiner: Semantic Manipulation, Analytics, Reporting and Terms"
    :version "0.1"
    :author "Ian Eslick <eslick@media.mit.edu>"
    :licence "BSD"
    :components ((:module "src"
			  ((:file "package")
			   (:file "csv-import")
			   (:file "classifiers")
			   (:file "umls")
			   (:file "bayes")
			   (:file "swirl")
			   (:file "naive-bayes")
			   (:file "utilities")
			   (:file "unifier")
			   (:file "syntax")
			   (:file "lam-utils")
			   (:file "lam-symptoms")
			   (:file "lam-patterns")
			   (:file "lam-lexical")
			   (:file "lam-lda")
			   (:file "lam-message-db")
			   (:file "lam-crf")
			   (:file "lam-crf-fields")
			   (:file "lam-crf-em")
			   (:file "lam-listserv-mining")
			   (:file "api")
			   (:file "lattice")
			   (:file "acor")
			   (:file "deident"))))
;;		 (:file "webtest"))
    :serial t
    :in-order-to ((load-op (compile-op :healthminer)))
    :depends-on (:stdutils :cl-json :hunchentoot :drakma :elephant :langutils :f-underscore
			   :trivial-shell :cxml :cxml-stp :cffi :closure-html
			   :clsql))

(defsystem #:healthminer-explorer    
    :version "0.1"
    :author "Ian Eslick <eslick@media.mit.edu>"
    :licence "BSD"
    :components ((:file "explorer"))
    :serial t
    :in-order-to ((load-op (compile-op :smart-explorer)))
    :depends-on (:smart :weblocks))
