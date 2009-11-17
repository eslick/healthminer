(in-package :smart)

;;
;; Parse the results
;;

(defun read-crf-labeling (result-filename)
  (let ((hash (make-hash-table))
	(msg-sentence nil))
    (with-open-file (stream result-filename :direction :input)
      (setf msg-sentence (read-labeled-sentence nil stream))
      (loop 
	 while msg-sentence
	 for message = (get-message-from-sentence msg-sentence)
	 do (setf (gethash message hash)
		  (mvbind (ms sentences) (get-message-sentences message stream)
		    (setf msg-sentence ms)
		    sentences))))
    hash))

(defun review-crf-labeling (result-filename)
  (mapcar #'print-crf-annotation
	  (all-sentence-annotations (read-crf-labeling result-filename)))
  t)

;;
;; All annotations
;;

(defun all-crf-labels (crf-hash &optional threshold)
  "Take outputs of CRF classifier in the hash"
  (loop for item in (hash-items crf-hash)
     nconc (all-sentence-crf-labels (cdr item) threshold)))

(defun all-sentence-crf-labels (crf-sentences &optional threshold)
  (mapcan #'(lambda (sentence)
	      (when (or (not threshold) (>= (length (annotations sentence)) threshold))
		(copy-list (annotations sentence))))
	  crf-sentences))

(defun print-crf-annotations (annotations &optional doc)
  (when (hash-table-p annotations)
    (setf annotations (all-sentence-annotations annotations)))
  (let ((last-msg nil))
    (loop for field in annotations
	 for current-msg = (message (sentence field))
       do 
	 (when (not (eq current-msg last-msg))
	   (setf last-msg current-msg)
	   (format t "~%")
	   (when doc (print-message-body current-msg)))
	 (print-crf-annotation field))))

(defun print-crf-annotation (field)
  (format t "~A: ~A~%" 
	  (type field)
	  (crf-field->string field)))

(defun crf-field->string (field)
  (concat-words 
   (subseq (words (sentence field))
	   (start field) (end field))))

(defun crf-field->ngram (field)
  (append (mapcar #'id-for-token 
		  (subseq (words (sentence field))
			  (start field) (end field)))
	  (list (type field))))

(defun filter-crf-labels-by-type (labels type)
  (select-if (f (label) (equalp (type label) type)) labels))

;;
;; Helpers
;;

(defun get-message-sentences (message stream)
  (loop 
     for sentence = (read-labeled-sentence message stream)
     while (cond ((null (words sentence))
		  (values nil sentences))
		 ((and (message-sentence-p sentence)
		       (= (length (words sentence)) 1))
		  (return (values sentence sentences)))
		 (t t))
     collect sentence into sentences))


(defclass crf-sentence ()
  ((words :accessor words :initarg :words)
   (message :accessor message :initarg :message)
   (annotations :accessor annotations :initarg :annotations :initform nil)))

(defun message-sentence-p (sentence)
  (and sentence
       (numberp (parse-integer (first (words sentence)) :junk-allowed t))))

(defun get-message-from-sentence (sentence)
  (get-message (parse-integer (first (words sentence)))))

(defclass crf-field ()
  ((sentence :accessor sentence :initarg :sentence)
   (type :accessor type :initarg :type)
   (start :accessor start :initarg :start)
   (end :accessor end :initarg :end)))

(defmethod print-object ((field crf-field) stream)
  (format stream "#<CRF-FIELD ~A '~A'>" (type field) (crf-field->string field)))

(defun crf-field-message (field)
  (message (sentence field)))

(defun read-labeled-sentence (message stream)
  (let* ((data (read-sentence stream))
	 (sentence (make-instance 'crf-sentence :words (cars data)
				  :message message)))
    (extract-fields sentence (cdrs data))
    sentence))


(defun extract-fields (sentence labels &aux field fields)
  (loop 
     for label in labels 
     for i from 0 do
     (if (nequal label (first field))
	 (if (null field)
	     (setf field (list label))
	     (progn
	       (push (make-crf-field sentence field i) fields)
	       (setf field (list label))))
	 (push label field)))
  (setf (annotations sentence)
	(flatten (remove-nulls (reverse (cons (make-crf-field sentence field (length labels)) fields))))))
;;  (break))


(defun make-crf-field (sentence list offset)
  (unless (or (null list) (null (first list))
	      (equal (first list) "OO") 
	      (equal (first list) "NA") 
	      (equal (first list) "NONE"))
    (make-instance 'crf-field
		   :sentence sentence
		   :type (first list)
		   :start (max (- offset (length list)) 0)
		   :end offset)))

(defun read-sentence (stream)
  (loop 
     for items = (extract-words (read-line stream nil nil))
     while (not (null items))
     collect (cons (first items) (last1 items))))

(defun crf-field-topics (field)
  "Get the list of topics associated with the field"
  (let ((topics (message-topics (message (sentence field)))))
    (when (< (end field) (length topics))
      (subseq (array->list topics) (start field) (end field)))))

(defun filter-crf-fields-by-topics (fields key-topics)
  "Pick all fields from the list that contain one or more of the provided topics"
  (if key-topics
      (select-if (f (field)
		   (some (f (ftopic) (member ftopic key-topics))
			 (crf-field-topics field)))
		 fields)
      fields))

(defun crf-field-window (field &optional (size 10))
  "Easy, but inefficient"
  (position-window (start field) size (words (sentence field))))
		     

;;
;; Pairing analysis
;;

(defun crf-neighbor-pairs (type1 type2 crf-hash &optional max)
  (let ((items (hash-items crf-hash)))
    (loop 
       for i from 0 upto (or max (length items))
       for (msg . sentences) in items
       nconc (crf-message-neighbor-pairs type1 type2 sentences))))

(defun crf-message-neighbor-pairs (type1 type2 sentences)
  (let* ((labels (all-sentence-crf-labels sentences))
	 (t1labels (filter-crf-labels-by-type labels type1))
	 (t2labels (filter-crf-labels-by-type labels type2))
	 (results (when (and (> (length t1labels) 0)
			     (> (length t2labels) 0)
			     (>= (+ (length t1labels) (length t2labels)) 2))
		    (gather-pairs t1labels t2labels))))
    results))

(defun print-crf-pairs (pairs)
  (labels ((get-string (elt)
	     (cond ((subtypep (type-of elt) 'phrase)
		    (phrase->string elt))
		   ((subtypep (type-of elt) 'crf-field)
		    (crf-field->string elt))
		  (t elt))))
  (dolist (pair pairs)
    (format t "~A -> ~A~%" (get-string (car pair)) (get-string (cdr pair))))))

(defun max-occurance-label-pairs (type1 type2 crf-hash &optional threshold)
  (let* ((labels (all-crf-labels crf-hash threshold))
	 (type1hist (histogram (filter-crf-labels-by-type labels type1)
			       :test #'equal :key #'crf-field->string))
	 (type2hist (histogram (filter-crf-labels-by-type labels type2) 
			       :test #'equal :key #'crf-field->string))
	 (pairs (crf-neighbor-pairs type1 type2 crf-hash)))
    (sort (loop for pair in pairs collect
	       (cons (+ (position (car pair) type1hist :test #'equal :key #'crf-field->string)
			(position (cdr pair) type2hist :test #'equal :key #'crf-field->string))
		     pair))
          #'< :key #'car)))
