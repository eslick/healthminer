(in-package :smart)


(defparameter *crf-classify-path* "crf_test")
(defparameter *training-filename* "/Users/eslick/temp/crf-training-set.dat")
(defparameter *model-filename* "/Users/eslick/temp/crf-model.dat")
(defparameter *testing-filename* "/Users/eslick/temp/crf-testing-set.dat")

(defun train-lam-crf (messages template-filename model-filename ngrams 
		      &key (topics t) (crf-hash nil) all-p (c-param 1.0))
  (write-crf-data-file messages *training-filename* ngrams 
		       :topics topics :crf-hash crf-hash :all-p all-p)
  (let* ((proc (trivial-shell::create-shell-process
	       (format nil "crf_learn -c ~A -p 4 -f 5 ~A ~A ~A" ;; -c 0.5 -t -p 2 -f 2
		       c-param
		       template-filename
		       *training-filename*
		       model-filename)
	       nil))
	 (out   (ccl::external-process-output-stream proc))
	 (error (ccl::external-process-error-stream proc)))
    (unwind-protect
	 (loop while (trivial-shell::process-alive-p proc) do
	      (progn
		(sleep 5)
		(loop for line = (read-line out nil)
		   while line
		   do (print line))))
      (close out)
      (close error))))


;;;
;;; Generate CRF Training Data
;;;

(defun write-crf-data-file (messages filename ngram-labels &key (topics t) crf-hash all-p)
  (let ((labeler (cond ((and ngram-labels crf-hash)
			(crf-field-labeler (ngrams-as-ids ngram-labels) crf-hash))
		       (ngram-labels 
			(crf-labeler (ngrams-as-ids ngram-labels)))
		       (t nil))))
    (with-output-file (stream filename)
      (mapc (f (msg) 
	      (write-crf-message stream msg labeler 
				 :use-topics topics 
				 :all-sentences all-p))
	    messages))
    t))

(defun write-crf-message (stream message labeler &key (use-topics t) all-sentences)
  "Given a stream, a labeler, and a set of messages"
  ;; Header
  (format stream "~A DOC ~@[~A~] ~@[~A~]~%~%" 
	  (get-message-id message) 
	  (when use-topics "TOP")
	  (when labeler "NONE"))
  ;; Dump sentence
  (let ((mdoc (message-doc message))
	(topics (and use-topics (message-topics message)))
	(labels (when labeler (phrase-label-message message labeler))))
    (dolist (sentence (message-sentences message))
      (when (or (not labels) all-sentences (sentence-has-label? sentence labels))
	(write-crf-sentence stream 
			    (phrase-start sentence)
			    (phrase-end sentence)
			    (document-text mdoc)
			    (document-tags mdoc)
			    topics
			    labels)))))

(defun sentence-has-label? (sentence labels)
  (loop for i from 0
        for l in labels
     do (when (and (> i (phrase-start sentence))
		   (< i (phrase-end sentence))
		   (not (null l)))
	  (return-from sentence-has-label? t))))

(defun write-crf-sentence (stream start end words tags topics labels)
  (loop for i from start upto end
       for label = (when labels (elt labels i))
       do (format stream "~A ~A ~@[~A~] ~@[~A~]~%"
;;		  (if label 
;;		      (random-string :length (length (get-lemma (token-for-id (aref words i)))))
		  (get-lemma (token-for-id (aref words i)))
		  (aref tags i)
		  (when topics (aref topics i))
		  (when labels (or label "NONE"))))
  (format stream "~%"))

(defun phrase-label-message (message labeler)
  "Return the list of labels for "
  (loop 
     for offset from 0 below (length (document-text (message-doc message)))
     collect (funcall labeler message offset)))

(defun ngrams-as-ids (ngram-labels)
  (collect 'ngram-as-ids ngram-labels))

(defun ngram-as-ids (ngram)
  (when ngram
    (let ((ids (mapcar #'id-for-token (subseq ngram 0 (1- (length ngram))))))
      (setf (nthcdr (1- (length ngram)) ids)
	    (list (last1 ngram)))
      ids)))

(defun ngram-as-string (ngram)
  (when (and (listp ngram) (numberp (first ngram)))
    (concat-words 
     (tokens-for-ids
      (subseq ngram 0 (1- (length ngram)))))))

(defun crf-labeler (ngram-labels &key (default nil))
  "Returns a labeler that given a vector and offset, returns a label or default.
   ngram-labels are of the form ('term' 'term' 'label').  Expects to be called
   linearly across the sequence"
  (let ((table (make-ngram-table ngram-labels))
	(current nil))
    (lambda (message i)
      (let* ((sequence (document-text (message-doc message)))
	     (word (elt sequence i)))
	(or (cond ((equal word (first current))
		   ;; If labeling an ngram, return the label and decrement
		   (prog1 (last1 current)
		     (setf current 
			   (if (= (length current) 2) nil
			       (rest current)))))
		  (t 
		   ;; Otherwise, find the matching n-gram
		   (loop for ngram in (gethash word table) do
			(when (ngram-match sequence i ngram) ;; matches
			  (when (> (length ngram) 2) ;; not unigram
			    (setf current (rest ngram))) ;; track labeling
			  (return (last1 ngram))))))
	    default)))))
  

(defun ngram-match (sequence offset ngram)
  (when (> (length sequence) (+ offset (length ngram)))
    (loop 
       for i from 0 below (1- (length ngram)) ;; don't count label
       unless (equal (elt sequence (+ offset i))
		     (elt ngram i))
       do (return nil)
       finally (return t))))
      
(defun make-ngram-table (ngrams)
  (loop 
     with table = (make-hash-table :test #'equal)
     for ngram in ngrams
     do (extend-ngram-table-entry ngram table)
     finally (return table)))

(defun extend-ngram-table-entry (ngram table)
  (labels ((ngram> (ng1 ng2) (> (length ng1) (length ng2))))
    (setf (gethash (first ngram) table)
	  (sort (cons ngram (gethash (first ngram) table)) #'ngram>))))

(defun document-text-remove-chunks (doc chunks)
  (loop with text = (document-text doc) 
     for offset from 0 
     for word across text
     when (not (some (curry 'phrase-contains-point offset) chunks))
     collect word))

;; CRF FIELD LABELING

(defun crf-field-labeler (ngram-labels crf-hash &optional default)
  (let ((ngram-labeler (crf-labeler ngram-labels))
	(message nil)
	(sequence nil)
	(msg-sentences nil)
	(crf-sentences nil)
	(msg-sentence nil)
	(crf-sentence nil))
    ;; Hairy way of keeping track because crf-hash doesn't easily connect to
    ;; the document-level offset indexing
    (labels ((setup (msg offset)
	       (unless (eq message msg)
		 (setf message msg)
		 (setf sequence (document-text (message-doc message)))
		 (setf msg-sentences (message-sentences message))
		 (setf msg-sentence nil)
		 (setf crf-sentences (gethash message crf-hash))
		 (setf crf-sentence nil))
	       (unless (and msg-sentence (phrase-contains-point offset msg-sentence))
		 (let ((pos (position-if (curry 'phrase-contains-point offset)
					 msg-sentences)))
		   (setf msg-sentence (nth pos msg-sentences))
		   (setf crf-sentence (nth pos crf-sentences))))))
      ;; Actual labeler
      (lambda (message offset)
	(setup message offset)
	(or (funcall ngram-labeler message offset)
	    (matching-crf-field-label offset msg-sentence crf-sentence)
	    default)))))

(defun matching-crf-field-label (msg-offset msg-sentence crf-sentence)
  (when crf-sentence
    (let ((fields (annotations crf-sentence))
	  (s-offset (- msg-offset (phrase-start msg-sentence))))
      (awhen (field-for-position s-offset fields)
	(type it)))))
	
(defun field-for-position (offset fields)
  (loop for field in fields
     when (and (>= offset (start field))
	       (< offset (end field)))
     return field))

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

(defun classify-lam-crf (messages model-filename result-filename &optional (topics t))
  (write-crf-data-file messages *testing-filename* nil :topics topics)
  (trivial-shell:shell-command 
   (format nil "crf_test -m ~A -o ~A ~A"
	   model-filename
	   result-filename
	   *testing-filename*)))


    
;;; chunk pairs

(defun crf-chunk-neighbor-pairs (type1 type2 crf-hash &optional max)
  (let ((items (hash-items crf-hash)))
    (loop 
       for i from 0 upto (or max (length items))
       for (msg . sentences) in items
       for pairs = (crf-message-neighbor-pairs type1 type2 sentences)
       when pairs
       nconc (convert-neighbor-pairs msg pairs crf-hash))))

(defun convert-neighbor-pairs (msg pairs crf-hash)
  (let ((map (chunks-matching-fields msg crf-hash)))
    (loop for pair in pairs
	 for left = (second (assoc (car pair) map))
	 for right = (second (assoc (cdr pair) map))
	 collect (cons (or left (car pair)) (or right (car pair))))))

(defun chunks-matching-fields (message crf-hash)
  "All chunks that contain a crf labeled field"
  (loop 
     with chunks = (message-chunks message)
     for msg-sentence in (message-sentences message) 
     for crf-sentence in (gethash message crf-hash)
     nconc
       (loop for field in (annotations crf-sentence) 
	  collect
	    (cons field (crf-field-matching-chunk field (phrase-start msg-sentence) chunks)))))

(defun crf-event-chunk-head-verbs (crf-hash)
;;  (remove-nulls 
   (mapcan (f (record)
	     (mapcar (f (phrase)
		       (when (subtypep (type-of phrase) 'phrase)
			 (phrase->string phrase)))
		     (rest record)))
	   (select-if (f (pair)
			(or (when (subtypep (type-of (car pair)) 'phrase)
			      (eq (phrase-type (car pair)) :event))
			    (when (subtypep (type-of (cdr pair)) 'phrase)
			      (eq (phrase-type (cdr pair)) :event))))
		      (mapcan (curry2 'chunks-matching-fields crf-hash) 
			      (get-instances-by-class 'message)))))

(defun crf-field-matching-chunk (field offset chunks)
  (loop for chunk in chunks 
     when (phrase-contains-point (+ offset (start field)) chunk)
     collect chunk))
	    

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
    

;;; OLD CRF WRITER

(defun write-crf-data-file-orig (messages filename &key topics msg-anno (annotations t) filter keywords)
  (with-output-file (stream filename)
    ;; for each message
    (loop for message in messages
	 for lines from 0 do
	 (let ((text-annos (when annotations 
			     (if filter
				 (filter-annotations filter (text-annotations message))
				 (text-annotations message))))
	       (msg-annos (when annotations (msg-annotations message)))
	       (lda-labels (when (eq topics :topics)
			     (message-topics message))))
	   (format stream "~A DOC ~@[~A~] ~@[~A~]~%~%" (get-message-id message)
		   (when topics "OO") (when annotations "OO"))
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

