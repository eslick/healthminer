(in-package :smart)

(defvar *symptoms-cxml* nil)

(defun get-symptom-cxml ()
  (aif-ret *symptoms-cxml*
    (setf *symptoms-cxml*
	  (cxml:parse
	   (drakma:http-request "http://purl.org/obo/owl/SYMP")
	   (stp:make-builder)))))

(defun get-symptom-classes ()
  (let ((hash (make-hash-table :test #'equal)))
    (xpath:map-node-set->list
     (lambda (node)
       (let ((string (stp:string-value node)))
	 (setf (gethash (id-for-token string) hash) string)))
     (xpath:with-namespaces (("owl" "http://www.w3.org/2002/07/owl#")
			     ("rdfs" "http://www.w3.org/2000/01/rdf-schema#"))
       (xpath:evaluate 
	"//owl:Class/rdfs:label"
	(get-symptom-cxml))))
    hash))

(defun formal-symptom-matches (symptom-strings onto-hash)
  (loop for string in symptom-strings collect
       (loop for word in (extract-words string)
	    when (gethash (id-for-token word) onto-hash)
	    collect word)))

(defun formal-symptoms-in-corpus (onto-hash)
  (let ((terms nil))
    (map-class (lambda (message)
		 (loop for term across (document-text (message-doc message)) 
		    when (gethash term onto-hash)
		    do (push term terms)))
	       'message)
   (remove-duplicates terms)))

(defun annotation-symptom-matches (type onto-hash)
  (formal-symptom-matches (mapcar 'text-annotation->string (all-text-annotations-by-type type))
			  onto-hash))