(in-package :smart)

;; =====================================
;; Expectation Maximization
;; =====================================

;; PRE-FILTER MATCHING UMLS NGRAMS
;; ADD OPTION TO WRITE ALL SENTENCES W-W/O LABELS
;; COMPUTE SEED SET
;; COMPUTE ITERATION

(defvar *all-messages* nil) 
(defvar *all-ngrams* nil)
(defvar *seed-labels* nil)
(defvar *matching-ngrams* nil)

(defparameter *result-filename* "~/temp/crf-result")

(defun run-crf-em (template &optional (iterations 10))
  "Assume topics, all messages, and umls ngrams.  Returns model file."
  (init-ngrams) (init-messages)
  (print "Training Seed Model and Dataset")
  (let ((crf-seed-hash (crf-train-and-classify template nil *result-filename* 
					       :c-param 0.25)))
    (values (iterate-crf-em template crf-seed-hash :iterations iterations)
	    *model-filename*)))

(defun iterate-crf-em (template labels &key (iterations 10))
  (dotimes (i iterations)
    (let ((*training-filename* (format nil "~~/temp/crf-training-set-~A.dat" i))
	  (*testing-filename* (format nil "~~/temp/crf-testing-set-~A.dat" i))
	  (*model-filename* (format nil "~~/temp/crf-model-~A.dat" i)))
      (declare (special *training-filename* *testing-filename* *model-filename*))
      (format t "Running CRF EM iteration #~A~%" i)
      (setf labels
	    (crf-train-and-classify template labels
				    (iteration-result-filename *result-filename* i)
				    :all t :c-param 1.0))))
  labels)
    

(defun crf-train-and-classify (template priors result-filename &key all c-param)
  "Train CRF on positive instances"
  ;; Train model on positive instances
  (train-lam-crf *all-messages* template *model-filename* *matching-ngrams* 
		 :topics :topics :crf-hash priors :all-p all :c-param c-param)
  ;; Label all data
  (classify-lam-crf *all-messages* *model-filename* result-filename :topics)
		    
  ;; Evaluate recall
  ;; (evaluate-labels *matching-ngrams*)
  ;; Return labeled data
  (read-crf-labeling result-filename))

(defun iteration-result-filename (base iteration)
  (format nil "~A-~A.dat" base iteration))

;; M Label unlabeled instances
;; E Train CRF on all instances
;; M Label all instances; print recall error
;; E Train CRF on all instances, force known to known

;; children(affects) =
;;   { causes, complicates, treats, prevents, manages, diagnoses }
;;     manifestation_of, indicates

;; R = children(affects)

;; C = { disorder, symptom/sign, procedure, 

;;
;; Utils and initialization
;;

(defun reset-em-data ()
  (setf *all-ngrams* nil)
  (setf *seed-labels* nil)
  (setf *matching-ngrams* nil)
  (setf umls::*procedures* nil)
  (setf umls::*symptoms* nil)
  (setf umls::*condition* nil))

(defun init-messages ()
  (unless *all-messages*
    (print "Loading all messages")
    (setf *all-messages* 
	  (get-instances-by-class 'message)))
  t)

(defparameter *blacklist*
  `(,(append (ids-for-tokens '("Release"))
	     (list 'umls::treat))))

(defun init-ngrams ()
  (print "Initializing reference data")
  ;; Ngrams
  (unless *all-ngrams*
    (print "Loading all ngrams from UMLS")
    (umls::setup-umls-training-ngrams)
    (setf *all-ngrams*
	  (set-difference
	   (append umls::*procedures*
		   umls::*symptoms*
		   umls::*condition*
		   (symptom-ngrams 
		    (get-symptom-classes)))
	   *blacklist*
	   :test #'equal)))

  ;; Compute seed labels
  (init-messages)
  (unless *seed-labels*
    (print "Computing seed label table from ngrams")
    (init-messages)
    (setf *seed-labels* 
	  (compute-reference-labels *all-messages* *all-ngrams*)))

  ;; Compute ngrams from corpus
  (unless *matching-ngrams*
    (print "Computing ngrams that match in corpus")
    (setf *matching-ngrams*
	  (mapcar #'crf-field->ngram 
		  (remove-duplicates
		   (flatten (mapcar (f (sentences)
				      (mapcar #'annotations sentences))
				    (hash-values *seed-labels*)))))))
  (print "Completed Initialization")
  t)

(defun compute-reference-labels (messages ngrams)
  (let ((hash (make-hash-table))
	(labeler (crf-labeler ngrams)))
    (loop for message in messages do
	 (progn
	   (loop for sentence in (message-sentences message) do
		(let* ((fields (sentence-reference-labels message sentence labeler))
		       (crf-sentence (make-instance 'crf-sentence 
						    :words (tokens-for-ids
							    (phrase-words sentence))
						    :message message
						    :annotations fields)))
		  (mapcar (f (field) (setf (sentence field) crf-sentence)) fields)
		  (push crf-sentence (gethash message hash))))
	   (setf (gethash message hash) 
		 (nreverse (gethash message hash)))))
    hash))

(defun sentence-reference-labels (message sentence labeler)
  (let ((label-sequence (label-sentence message sentence labeler))
	(last-label nil)
	(offset 0)
	(start nil)
	(labels nil))
    ;; FSM field extractor
    (labels ((recog (label)
	       ;; End of field
	       (when (and last-label (neq last-label label))
		 (push (make-instance 'crf-field :type last-label
				      :start start :end offset :sentence nil)
		       labels)
		 (setf start nil last-label nil))
	       ;; Start of field
	       (when (and (not last-label) label)
		 (setf last-label label)
		 (setf start offset))
               (incf offset)))
      (mapcar #'recog label-sequence)
      labels)))
	 
(defun label-sentence (message sentence labeler)
  (loop for i from (phrase-start sentence) upto (phrase-end sentence) 
     collect (funcall labeler message i)))


