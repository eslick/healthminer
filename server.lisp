;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: lamsight -*-

(in-package :lamsight)

(defparameter *clsql-server-spec*
  '(
  
(defun test-setup ()
  (clsql-sys:push-library-path "/usr/local/pgsql/lib/"))

(defun connect-to-backend ()
  (clsql:connect '()))
  