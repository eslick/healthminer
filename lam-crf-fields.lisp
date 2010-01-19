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

(defun sort-crf-ngrams (labels)
  (histogram (mapcar #'crf-field->string labels) :test #'equal))

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

(defun crf-field-topics (field crf-hash)
  "Get the list of topics associated with the field"
  (let ((topics (message-topics (message (sentence field))))
	(range (crf-field-message-range field crf-hash)))
    (dbind (start end) range
      (when (< end (length topics))
	(subseq topics start end)))))

(defun filter-crf-fields-by-topics (fields key-topics)
  "Pick all fields from the list that contain one or more of the provided topics"
  (if key-topics
      (select-if (f (field)
		   (some (f (ftopic) (member ftopic key-topics))
			 (crf-field-topics field)))
		 fields)
      fields))

;;
;; Differences between two field sets
;;

(defun crf-field-equal (field1 field2)
  (equal (crf-field->ngram field1)
	 (crf-field->ngram field2)))

(defun diff-crf-labels (hash1 hash2)
  "Returns elements in hash1 or hash2 not in the other"
  (let ((dropped nil)
	(added nil))
    (loop 
       for message in *all-messages* 
       for sentences1 = (gethash message hash1)
       for sentences2 = (gethash message hash2)
       when (and sentences1 sentences2)
       do (let ((labels1 (all-sentence-crf-labels sentences1))
		(labels2 (all-sentence-crf-labels sentences2)))
	    (push (set-difference labels1 labels2 :test #'crf-field-equal) dropped)
	    (push (set-difference labels2 labels1 :test #'crf-field-equal) added)))
    (setf dropped (flatten dropped))
    (setf added (flatten added))

    (values (+ (length dropped) (length added)) dropped added)))

;;
;; Fields matching seed ngrams
;;

(defun unique-fields (fields ngrams)
  "Remove all fields matching any of the strings"
  (let ((strings (ngrams-as-strings ngrams)))
     (remove-if (f (field) (member (crf-field->string field) strings :test #'equal))
		fields)))

(defun unique-concepts (fields ngrams)
  (remove-duplicates (mapcar #'crf-field->string 
			     (unique-fields fields ngrams))))

(defun ngrams-as-strings (ngrams)
  (mapcar #'ngram-as-string ngrams))

;;
;; Locating fields in messages
;;

(defun crf-field-message-range (field crf-hash)
  (let* ((message (crf-field-message field))
	 (sentences (gethash message crf-hash))
	 (sstart (crf-sentence-msg-start (sentence field) sentences)))
    (list (+ sstart (start field)) (+ sstart (end field)))))

(defun crf-sentence-msg-start (sentence sentences)
  (labels ((find-start (offset sentences)
	     (assert sentences)
	     (if (eq sentence (car sentences))
		 offset
		 (find-start (+ offset (length (words (first sentences))))
			     (rest sentences)))))
    (find-start 0 sentences)))

(defun crf-sentence-msg-end (sentence sentences)
  (+ (length (word sentence))
     (crf-sentence-msg-start sentence sentences)))
	  

;;
;; Verb analysis
;;

(defun crf-field-verb-adjacencies (message crf-hash &key (window 10))
  (let ((crf-sentences (gethash message crf-hash)))
    (loop for msg-sentence in (message-sentences message)
	  for crf-sentence in crf-sentences
	  for annotations = (annotations crf-sentence)
	  when annotations nconc
	 (crf-fields-window-verbs msg-sentence annotations window))))

;;(defun crf-fields-window-verbs (msg-sentence fields window)
;;  (phrase-verbs msg-sentence)

(defun phrase-verbs (phrase annotations window)
  (let* ((doc (phrase-document phrase))
	 (text (document-text doc))
	 (tags (document-tags doc)))
    (loop for i from (phrase-start phrase) upto (phrase-end phrase)
       for pos = (aref tags i) 
       when (verb-pos-p pos)
       collect (cons (aref text i) pos))))



;;
;; Lexical windows
;;

(defun crf-field-lexical-window (field &optional (size 10))
  "Easy, but inefficient"
  (position-window (start field) size (words (sentence field))))

(defun print-crf-field-pair-window (field1 field2 &optional (size 20) (stream t))
  (format stream "~A ... ~A~%" 
	  (crf-field-lexical-window field1 (/ size 2))
	  (crf-field-lexical-window field2 (/ size 2))))
;;
;; Relational windows
;;

(defun crf-neighbor-pairs (type1 type2 crf-hash &optional max (size 2))
  (let ((items (hash-items crf-hash)))
    (loop 
       for i from 0 upto (or max (length items))
       for (msg . sentences) in items
       nconc (crf-message-neighbor-pairs type1 type2 sentences size))))

(defun crf-message-neighbor-pairs (type1 type2 sentences &key (size 2))
  (loop for i from 0 upto (min (- (length sentences) size) (length sentences))
     collect (crf-sentence-window-neighbor-pairs 
	      type1 type2 
	      (subseq sentences i (+ i size)))))

(defun crf-sentence-window-neighbor-pairs (type1 type2 sentences)
  (let* ((labels (all-sentence-crf-labels sentences))
	 (t1labels (filter-crf-labels-by-type labels type1))
	 (t2labels (filter-crf-labels-by-type labels type2))
	 (results (when (and (> (length t1labels) 0)
			     (> (length t2labels) 0)
			     (>= (+ (length t1labels) (length t2labels)) 2))
		    (gather-pairs t1labels t2labels))))
    results))

(defun all-crf-pairs (crf-hash)
  (mappend #'crf-pairs (hash-values crf-hash)))

(defun crf-pairs (sentences)
  (let ((labs (all-sentence-crf-labels sentences)))
    (labels ((gather (label rest)
	       (when rest
		 (if (equal (crf-field->string label)
			    (crf-field->string (first rest)))
		     (gather (car rest) (rest rest))
		     (cons (cons label (car rest))
			   (gather (car rest) (rest rest)))))))
      (when (>= (length labs) 2)
	(gather (car labs) (rest labs))))))

(defun print-crf-pairs (pairs &key (stream t))
  (labels ((get-string (elt)
	     (cond ((subtypep (type-of elt) 'phrase)
		    (phrase->string elt))
		   ((subtypep (type-of elt) 'crf-field)
		    (crf-field->string elt))
		  (t elt))))
    (dolist (pair pairs)
      (format stream "~A: ~A -> ~A~%" (get-message-id (message (sentence (car pair))))
	      (get-string (car pair)) (get-string (cdr pair))))))

(defun max-occurance-label-pairs (type1 type2 crf-hash &optional threshold)
  (let* ((labels (all-crf-labels crf-hash threshold))
	 (type1hist (histogram (filter-crf-labels-by-type labels type1)
			       :test #'equalp :key #'crf-field->string))
	 (type2hist (histogram (filter-crf-labels-by-type labels type2) 
			       :test #'equalp :key #'crf-field->string))
	 (pairs (crf-neighbor-pairs type1 type2 crf-hash)))
    (sort (loop for pair in pairs collect
	       (cons (+ (position (crf-field->string (car pair)) type1hist 
				  :test #'equalp :key #'car)
			(position (crf-field->string (cdr pair)) type2hist 
				  :test #'equalp :key #'car))
		     pair))
          #'< :key #'car)))

