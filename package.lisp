(defpackage :smart
  (:use :cl :stdutils :hunchentoot :json :elephant :langutils))

(defpackage :smart-lex
  (:use :cl :stdutils :langutils)
  (:export :lexpat-match :lexpat-matches))

