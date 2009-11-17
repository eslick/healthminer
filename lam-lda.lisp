(in-package :smart)



;;
;; Write Mallet training file
;;

(defparameter *mallet-path* 
  "/Users/eslick/Desktop/Downloads/mallet-2.0-RC4/bin/mallet")

(defun run-mallet (messages temp-name mallet-name num-topics iterations
			state-name keys-name inferencer-name 
		   &key dump-p run-p sentencesp)
  (when dump-p
    (dump-for-mallet messages temp-name mallet-name :sentencesp sentencesp))
  (when run-p
    (mallet-topic-train mallet-name num-topics iterations state-name keys-name inferencer-name)))

(defun mallet-topic-train (mallet-name num-topics iterations 
			   state-name keys-name inferencer-name)
  (trivial-shell:shell-command
   (format nil "~A train-topics --input ~A --num-topics ~A --num-iterations ~A --output-state ~A --output-topic-keys ~A --num-top-words 100 --inferencer-filename ~A";; --num-threads 4 --optimize-interval 10"
	   *mallet-path* mallet-name num-topics iterations 
	   state-name keys-name inferencer-name)))

(defun dump-for-mallet (messages temp-name mallet-name &key (import-p t) sentencesp)
  (with-output-file (stream temp-name)
    (loop for message in messages do
	 (dump-mallet-message stream message sentencesp)))
  (when import-p
    (trivial-shell:shell-command
     (format nil "~A import-file --input ~A --output ~A --keep-sequence --remove-stopwords"
	     *mallet-path* temp-name mallet-name))))

(defun dump-mallet-message (stream message &optional sentences)
  (if sentences
      (let ((sentences (message-sentences message)))
	(loop for sentence in sentences
	     for index from 0 do
	     (format stream "~A-~A ~{~A ~}~%" 
		     (get-message-id message) index
		     (cleaned-divisi-tokens (phrase-words sentence)))))
      (format stream "~A ~{~A ~}~%" 
	      (get-message-id message)
	      
	      (cleaned-divisi-tokens (message-words message)))))

(defun mallet-quick-run (messages iterations &key (dump t) sentencesp)
  (run-mallet messages 
	      "/Users/eslick/temp/topic.raw"
	      "/Users/eslick/temp/topic.mallet"
	      100
	      iterations
	      "/Users/eslick/temp/topic.state.gz"
	      "/Users/eslick/temp/topic.keys"
	      "/Users/eslick/temp/topic.inferencer"))


;;
;; Read key table
;;

(defun read-key-file (filename)
  (let ((hash (make-hash-table :test #'equal)))
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil)
	   do (if (null line)
		  (return hash)
		  (destructuring-bind (id alpha &rest words) 
		      (extract-words line)
		    (setf (gethash (parse-integer id) hash)
			  (cons words (parse-number:parse-number alpha)))))))))

(defun terms-in-key-table (terms table)
  (let ((cats (sort (mapcar #'(lambda (pair)
				(list (car pair)
				      (cddr pair)
				      (cadr pair)))
			    (hash-items table))
		    #'> :key #'second)))
    (labels ((term-in-table (term)
	       (loop for cat in cats
		  when (member term (third cat) :test #'equal)
		  return (first cat))))
      (loop 
	 with matches = nil
	 for term in (flatten (mapcar #'extract-words terms ))
	 for id = (term-in-table term)
	 when id do (push (cons term id) matches)
	 finally (return (nreverse matches))))))

(defun read-topic-label-file (filename)
  (let ((hash (make-hash-table)))
    (with-open-file (stream filename)
      (do-stream-lines (line stream)
	(unless (eq (aref line 0) #\#)
	  (dbind (topic label &rest rest)
	      (extract-words line)
	    (declare (ignore rest))
	    (hash-put hash (parse-integer topic) label)))))
    hash))

(defun get-topic-label (topic label-table)
  (if label-table
      (let ((label (gethash topic label-table)))
	(unless (equal label "CRUFT") label))
      topic))

(defun message-annotation-topic-distribution (topic-labels)
  "For each message-level annotation, capture the topic histogram"
  (mapcar (f (annotation) 
	    (cons (type annotation) 
		  (cons (when (text-annotations (message annotation)) t)
			(select-if (f (topic) (and (stringp (car topic))
						   (not (equal (car topic) "CRUFT"))))
				   (topic-distribution (message annotation) topic-labels)))))
	  (get-instances-by-class 'msg-annotation)))
       
	 
;;
;; Read LDA labeling
;;


(defun read-lda-messages-from-state-file (filename &key limit fn)
  (with-open-file (stream filename)
    (loop 
       with records = nil
       with mrec = nil
       with mid = 0
       for i from 0 
       for line = (read-line stream nil)
       do (cond ((or (> i limit) (null line))
		 (return (nreverse records)))
		((eq (char line 0) #\#)
		 (continue))
		(t (multiple-value-bind (result id)
		       (parse-lda-message-line line)
		     (when (not (eq mid id))
		       (setf mid id)
		       (push (nreverse mrec) records)
		       (setf mrec nil))
		     (push result mrec)))))))

(defun lda-state-file-reader (filename)
  (let ((stream (open filename))
	(last-line nil)
	(mrec nil)
	(skip 0)
	(mid 0))
      (lambda (&optional finish)
	(handler-case 
	    (cond (finish (close stream))
		  ((> skip 0)
		   (decf skip)
		   nil)
		  (t (loop 
			for line = (or last-line (read-line stream t))
			unless (eq (char line 0) #\#)
			do 
			(multiple-value-bind (result id)
			    (parse-lda-message-line line)
			  (when last-line (setf last-line nil))
			  (if (not (eq mid id))
			      (progn 
				(setf skip (- id mid 1))
				(setf mid id)
				(setf last-line line)
				(return
				  (prog1 (nreverse mrec)
				    (setf mrec nil))))
			      (push result mrec))))))
	  (error () (close stream) nil)))))

(defun parse-lda-message-line (line)
  (dbind (mid label offset wid wstring topic) 
      (extract-words line)
    (declare (ignore label offset wid))
    (values (cons wstring (parse-integer topic)) (parse-integer mid))))

(defmacro with-lda-reader ((var filename) &body body)
  (assert (symbolp var))
  `(let ((,var (lda-state-file-reader ,filename)))
     (unwind-protect
	  ,@body
       (funcall ,var t))))

(defun make-lda-message-annotation-vector (message lda-annotations)
  (loop 
     with doc = (document-text (message-doc message))
     with annos = lda-annotations
     with avec = (make-array (length doc) :adjustable t :fill-pointer 0)
     for i from 0
     while annos
     do (dbind (word . topic) (first annos)
;;	  (format t "~A ~A ~%" (token-for-id (aref doc i)) (first annos))
	  (mvbind (match-p processed) (lda-token-match-p (aref doc i) word)
	    (loop for term in processed
	       do (when (equalp term (car (first annos))) (pop annos)))
	    (vector-push-extend (if match-p topic "NA") avec)))
     finally (return avec)))
	     
(defun lda-token-match-p (id word)
  (let* ((raw (token-for-id id))
	 (position (position-if-not #'alpha-char-p raw))
	 (processed nil))
    (if (and position (> (length raw) 1))
	(loop 
	   with start = 0
	   while (and start position)
	   do (progn
		(push (subseq raw start position) processed)
;;		(format t "~A ~A ~A~%" (first processed) start position)
		(setf start (position-if #'alpha-char-p raw :start position))
		(when start
		  (setf position (position-if-not #'alpha-char-p raw :start start))
		  (when (null position)
		    (push (subseq raw start) processed)))))
;;		    (format t "~A ~A ~A~%" (first processed) start position)))))
	(push raw processed))
    (values (equalp (last1 processed) word) (reverse processed))))
       