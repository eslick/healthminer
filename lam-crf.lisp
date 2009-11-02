(in-package :smart)


;; Quick hack to use CRF++ to perform corpus extraction


(defparameter *crf-classify-path* "crf_test")
(defparameter *training-filename* "/Users/eslick/temp/crf-training-set.dat")
(defparameter *model-filename* "/Users/eslick/temp/crf-model.dat")
(defparameter *testing-filename* "/Users/eslick/temp/crf-testing-set.dat")


(defun run-crf (template training testing output-file &key terms filter)
  (print "Training...")
  (train-lam-crf training template *model-filename* terms filter)
  (print "Classifying...")
  (classify-lam-crf testing *model-filename* output-file terms))

(defun train-lam-crf (messages template-filename model-filename &optional terms filter (annotations t))
  (write-crf-data-file messages *training-filename* :annotations annotations
		       :msg-anno nil :terms terms :filter filter)
  (trivial-shell:shell-command 
   (format nil "crf_learn ~A ~A ~A"
	   template-filename
	   *training-filename*
	   model-filename)))

(defun write-crf-data-file (messages filename &key (annotations t) msg-anno terms filter)
  (with-output-file (stream filename)
    ;; for each message
    (loop for message in messages
	 for lines from 0 do
	 (let ((text-annos (when annotations 
			     (if filter
				 (filter-annotations filter (text-annotations message))
				 (text-annotations message))))
	       (msg-annos (when annotations (msg-annotations message)))
	       (lda-labels (when (eq terms :topics)
			     (message-topics message))))
	   (format stream "~A DOC ~@[~A~] ~@[~A~]~%~%" (get-message-id message)
		   (when terms "OO") (when annotations "OO"))
	   ;; For each sentence
	   (loop 
	      for sentence in (document-sentence-phrases (message-doc message))
	      for snum from 0 
	      for mtype = (and msg-anno (sentence-message-annotation msg-annos snum))
	      when (or (not annotations) 
		       (eq annotations :all)
		       (annotations-for-phrase message sentence filter))
	      do
		;; For each token
		(loop 
		   for id in (phrase-words sentence)
		   for offset from (phrase-start sentence)
		   do (format stream "~A ~A ~@[~A~] ~@[~A~]~%"
			      (token-for-id id)
			      (aref (document-tags (phrase-document sentence)) offset)
			      (cond ((null terms) nil)
				    ((listp terms) (if (gethash id terms) "DOM" "OO"))
				    ((eq terms :topics)
				     (if (>= offset (length lda-labels))
					 "NA"
					 (aref lda-labels offset))))
			      (and annotations
				   (or (first (term-phrase-annotation text-annos offset filter))
				       (or mtype "OO")))))
		(format stream "~%")))
       finally (print lines))))

(defun filter-annotations (types annotations)
  (select-if (f_ (member (type _) types)) annotations))

(defun sentence-message-annotation (annos snum)
  (loop 
     for anno in annos
     do
       (with-slots (range type) anno
	 (cond ((null range) (return type))
	       ((eq snum range) (return type))
	       ((and (consp range)
		     (>= snum (first range))
		     (<= snum (second range)))
		(return type))
	       (t nil)))))

(defparameter *remap-annotation-types* nil)

(defun term-phrase-annotation (annos term-offset &optional filter)

  (loop
     for anno in annos 
     when (or (not filter)
	      (member (type anno) filter))
     when (contained? term-offset (start anno) (end anno))
     collect (maybe-remap-anno-type (type anno))))

(defun maybe-remap-anno-type (type)
  (if *remap-annotation-types*
      (aif (assoc-get type *remap-annotation-types*)
	   it type)
      type))
	      
(defmethod contained? ((obj number) start end)
  (and (>= obj start)
       (<= obj end)))
				
;;
;; Run the classifier
;;

(defun classify-lam-crf (messages model-filename result-filename &optional terms)
  (write-crf-data-file messages *testing-filename* :annotations nil :terms terms)
  (trivial-shell:shell-command 
   (format nil "crf_test -m ~A -o ~A ~A"
	   model-filename
	   result-filename
	   *testing-filename*)))

;;
;; Parse the results
;;

(defun read-crf-labeling (result-filename)
  "Asssumes same messages in same order as used to generate testing file"
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
		(annotations sentence)))
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
	  (concat-words 
	   (subseq (extract-words (words (sentence field)))
		   (start field) (end field)))))

(defun filter-crf-labels-by-type (labels topic)
  (select-if (f (label) (eq (type label) topic)) labels))

;;
;; Helpers
;;

(defun get-message-sentences (message stream)
  (loop 
     for sentence = (read-labeled-sentence message stream)
     while (cond ((null (words sentence))
		  (values nil sentences))
		 ((and (message-sentence-p sentence)
		       (= (length (extract-words (words sentence))) 1))
		  (return (values sentence sentences)))
		 (t t))
     collect sentence into sentences))


(defclass crf-sentence ()
  ((words :accessor words :initarg :words)
   (message :accessor message :initarg :message)
   (annotations :accessor annotations :initarg :annotations :initform nil)))

(defun message-sentence-p (sentence)
  (and sentence
       (numberp (parse-integer (words sentence) :junk-allowed t))))

(defun get-message-from-sentence (sentence)
  (get-message (parse-integer (words sentence))))

(defclass crf-field ()
  ((sentence :accessor sentence :initarg :sentence)
   (type :accessor type :initarg :type)
   (start :accessor start :initarg :start)
   (end :accessor end :initarg :end)))

(defmethod print-object ((field crf-field) stream)
  (format stream "#<CRF-FIELD ~A ~A>" (type field) (- (end field) (start field))))

(defun read-labeled-sentence (message stream)
  (let* ((data (read-sentence stream))
	 (sentence (make-instance 'crf-sentence :words (concat-words (cars data))
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
  (remove-nulls (nreverse (cons (make-crf-field sentence field (length labels)) fields))))

(defun make-crf-field (sentence list offset)
  (unless (or (null list) (null (first list)) (equal (first list) "OO"))
    (push (make-instance 'crf-field
			 :sentence sentence
			 :type (first list)
			 :start (max (- offset (length list)) 0)
			 :end offset)
	  (annotations sentence))))

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

;;
;; Pairing analysis
;;

(defun crf-neighbor-pairs (type1 crf-hash1 type2 crf-hash2)
  (loop 
     for (msg . sentences) in crf-hash1 
     for (msg2 . sentences2) = (gethash msg crf-hash2)
     nconc
       (gather-pairs (filter-crf-labels-by-type 
		      (all-sentence-crf-labels sentences) type1)
		     (filter-crf-labels-by-type
		      (all-sentence-crf-labels sentences2) type2))))

;;
;; Topic impact experiment
;;

(defun quick-run-crf-topic-experiment (term-filter key-topics-filter)
  "Assuming annotated for training and diff of messages and annotated for testing"
  (declare (special messages annotated))
  (let ((hash (run-crf-topic-experiment annotated (set-difference messages annotated) 
					term-filter "~/temp/crf-results2.dat")))
    (cons (filter-crf-fields-by-topics (all-sentence-annotations hash) key-topics-filter)
	  hash)))

(defun run-crf-topic-experiment (training testing filter output)
  (train-lam-crf training "~/temp/template-dom" *model-filename* :topics
		 filter)
  (classify-lam-crf testing *model-filename* output :topics)
  (read-crf-labeling output))
    