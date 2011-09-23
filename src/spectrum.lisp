(in-package :smart)

(defparameter *topic-keys* nil)
(defparameter *topic-labels* nil)

(defun load-topics ()
  (setf *topic-keys* 
	(read-key-file "~/temp/200topics-full/topic.keys"))
  (setf *topic-labels* 
	(read-topic-label-file "~/temp/200topics-full/topic.labels")))

(define-api-handler "/spectrum" (method params json)
  (serialize-topic-table))

(defun serialize-topic-table ()
  (let ((results (make-hash-table))
	(id 0)
	(roots nil))
    (maphash (f (tid record)
		(dbind (terms prior empty) record
		  (declare (ignore empty))
		  (let ((this (incf id))
			(label (get-topic-label tid *topic-labels*)))
		    (when label
		      (push this roots)
		      (setf (gethash this results)
			    `(("id" . ,this)
			      ("topic" . ,tid)
			      ("weight" . ,prior)
			      ("term" . ,label)
;;			      ("term" . "")
			      ("children" ,(1+ this))))
		      (dolist (term (safe-subseq terms 0 40))
			(let ((this (incf id)))
			  (setf (gethash this results)
				`(("id" . ,this)
				  ("term" . ,term)
				  ("children" ,(1+ this))))))
		      (setf (cdr (assoc "children" (gethash id results) :test #'equal)) nil)))))
	      *topic-keys*)
    (setf (gethash 0 results)
	  `(("id" . 0)
	    ("term" . "")
	    ("children" ,@roots)))
    (hash-values results)))
   