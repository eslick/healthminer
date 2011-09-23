(defpackage :smart
  (:use :cl :stdutils :hunchentoot :json :elephant :langutils :f-underscore :cffi
	:clsql)
  (:shadowing-import-from :json :with-array)) 

(defpackage :smart-lex
  (:use :cl :stdutils :langutils)
  (:export :lexpat-match :lexpat-matches))

