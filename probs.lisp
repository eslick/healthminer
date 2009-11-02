(in-package :smart)

(defstruct cond-prob dep cond)

(defmethod print-object ((obj cond-prob) stream)
  (format stream "P(~{~A,^ ~}|~{~A,~})" (cond-prob-dep obj) (cond-prob-cond obj)))

(defun make-cp (dep cond) (make-cond-prob :dep dep :cond cond))

(defmacro P (&rest args)
  (let* ((position (position '! args))
	 (deps (if (null position)
		   args
		   (subseq args 0 position)))
	 (conds (if (null position) nil
		    (subseq args (1+ position)))))
    `(make-cp ',deps ',conds)))

