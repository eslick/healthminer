;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: lamsight -*-

(in-package :lamsight)

(defparameter *django-settings-module* "lamsight.settings")
(defparameter *lamsight-path* "/Users/eslick/Work/lamsight/")
(defparameter *lisp-models-path* (concatenate 'string *lamsight-path* "lisp_models.py"))

(defun setup-model-import ()
  (setf (port:getenv "DJANGO_SETTINGS_MODULE") *django-settings-module*)
;;  (setf (port:getenv "PYTHONPATH") 
;;	(concatenate 'string *lamsight-path* ":" (port:getenv "PYTHONPATH")))
  )

(defmethod shell-kill (pid)
  (port:run-prog "kill" :wait t :args (list "-9" (format nil "~A" pid))))

(defun load-lamsight-models ()
  (loop 
     for model in (get-lamsight-models) 
     do (eval model)))

(defun get-lamsight-models (&aux models)
  (setup-model-import)
  (multiple-value-bind (stream empty id) 
      (port:pipe-input *lisp-models-path*)
    (declare (ignore empty))
    (handler-case
	(loop 
	   (let ((expr (read stream)))
	     (format t "Reading model: ~A~%" (second expr))
	     (push expr models)))
      (end-of-file () ()))
    (close stream)
    (shell-kill id))
  models)
    