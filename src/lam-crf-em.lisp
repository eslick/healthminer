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

(defun run-crf-em (template &optional (iterations 10) (half nil))
  "Assume topics, all messages, and umls ngrams.  Returns model file."
  (init-ngrams) (init-messages)
  (print "Training Seed Model and Dataset")
  (let ((crf-seed-hash (crf-train-and-classify template nil *result-filename* 
					       :c-param 0.5 :all nil :half half)))
    (values (iterate-crf-em template crf-seed-hash :iterations iterations)
	    *model-filename*)))

(defun iterate-crf-em (template labels &key (iterations 10))
  (dotimes (i iterations)
    (let ((*training-filename* (format nil "~~/temp/crf-training-set-~A.dat" i))
	  (*testing-filename* (format nil "~~/temp/crf-testing-set-~A.dat" i))
	  (*model-filename* (format nil "~~/temp/crf-model-~A.dat" i)))
      (declare (special *training-filename* *testing-filename* *model-filename*))
      (format t "Running CRF EM iteration #~A~%" i)
      (let ((new-labels (crf-train-and-classify template labels
				    (iteration-result-filename *result-filename* i)
				    :all t :c-param 0.5)))
	(print (length (diff-crf-labels labels new-labels)))
	(setf labels new-labels))))
  labels)


(defun crf-train-and-classify (template priors result-filename &key all c-param half)
  "Train CRF on positive instances"
  ;; Train model on positive instances
  (train-lam-crf (if half (subseq *all-messages* 0 20000)
		     *all-messages*)
		 template *model-filename* *matching-ngrams* 
		 :topics :topics :crf-hash priors :all-p all :c-param c-param)
  ;; Label all data
  (classify-lam-crf (if half (subseq *all-messages* 20000)
			*all-messages*)
		    *model-filename* result-filename :topics)
		    
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

(defparameter *hand-ngrams*
  '((88175 TREAT) (152630 4395 TREAT) (152630 38281 TREAT) (593484 TREAT) (7734 TREAT) (593430 SYMP) (714441 SYMP) (586327 SYMP) (82055 COND) (418796 COND) (309797 SYMP) (24933 SYMP) (119829 SYMP) (147726 601621 TREAT) (127893 181687 COND) (418796 138271 COND) (596921 COND) (595517 TREAT) (600193 TREAT) (595490 SYMPT) (191494 595490 SYMPT) (406619 TREAT) (55513 95029 SYMPT) (37090 17999 SYMPT)))

(defparameter *hand-strings*
  '("oxygen" "lung surgery" "lung transplant" "tx" "exercise" "pneumo" "lung collapse" "wheeze" "asthma" "lam" "chyle" "nausea" "dizziness" "breath execise" "heart disease" "lam cell" "pleurectomy" "rapamycin" "sirolimus" "PFT" "stable PFT" "inhaler" "weight gain" "feel better"))

(defun reset-em-data (&optional umls)
  (setf *all-ngrams* nil)
  (setf *seed-labels* nil)
  (setf *matching-ngrams* nil)
  (when umls
    (setf umls::*procedures* nil)
    (setf umls::*symptoms* nil)
    (setf umls::*condition* nil)))

(defun init-messages ()
  (unless *all-messages*
    (print "Loading all messages")
    (setf *all-messages* 
	  (get-instances-by-class 'message)))
  t)

(defparameter *blacklist* nil)

(defun get-blacklist ()
  (unless *blacklist*
    (setf *blacklist*
	  (list (append (ids-for-tokens '("Release"))
			(list 'treat))
		(append (ids-for-tokens '("disease"))
			(list 'cond))
		(append (ids-for-tokens '("catch"))
			(list 'sympt)))))
  *blacklist*)

(defparameter *whitelist* nil)

(defun get-whitelist ()
  (unless *whitelist*
    (setf *whitelist*
	  (list (append (ids-for-tokens '("oxygen"))
			(list 'treat))
		(append (ids-for-tokens '("pneumothorax"))
			(list 'cond))))))

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
		   (ngrams-as-ids 
		    (symptom-ngrams 
		     (get-symptom-classes)))
		   (get-whitelist))
	   (get-blacklist)
	   :test #'equal)))

  ;; Compute seed labels
  (init-messages)
  (unless *seed-labels*
    (time 
     (progn
       (print "Computing seed label table from ngrams")
       (init-messages)
       (setf *seed-labels* 
	     (compute-reference-labels *all-messages* *all-ngrams*)))))

  ;; Compute ngrams from corpus
  (unless *matching-ngrams*
    (time 
     (progn 
       (print "Computing ngrams that match in corpus")
       (setf *matching-ngrams*
	     (remove-duplicates
	      (mapcar #'crf-field->ngram 
		      (flatten (mapcar (f (sentences)
					 (mapcar #'annotations sentences))
				       (hash-values *seed-labels*))))
	      :test #'equalp)))))
  (print "Completed Initialization")
  t)

(defparameter *last-message* nil)

(defun compute-reference-labels (messages ngrams)
  (let ((hash (make-hash-table))
	(labeler (crf-labeler ngrams)))
    (awhen (and *last-message* (position *last-message* messages))
      (setf messages (subseq messages it)))
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


