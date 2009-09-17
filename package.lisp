(defpackage :smart
  (:use :cl :stdutils :hunchentoot :json :elephant :langutils :f-underscore)
  (:shadowing-import-from :json :with-array)) 

(defpackage :smart-lex
  (:use :cl :stdutils :langutils)
  (:export :lexpat-match :lexpat-matches))

