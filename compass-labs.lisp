(in-package :smart)

;; WORD COUNTS

(defparameter *tweet-input-file* "/Users/eslick/Downloads/TweetSample3.dat")

(defun word-counts (&optional (file *tweet-input-file*) filters)
  (let* ((filtered-samples (get-filtered-samples file filters))
	 (counts (make-hash-table :test #'equalp)))
    (mapcar (curry2 #'count-words counts) 
	    (mapcar #'concat-words (cars filtered-samples)))
    (sort (hash-items counts) ;; (filter-if #'string-stopword? (hash-items counts) :key #'car)
	  #'> :key #'cdr)))

(defun frequency-ratios (counts1 counts2)
  "counts1 / counts2"
  (sort 
   (loop for (string . count) in counts1 
      for c2count = (find-count string counts2)
      collect (cons string (if c2count (coerce (/ count c2count) 'float)
			       count)))
   #'> :key #'cdr))

(defun find-count (word counts)
  (awhen (find word counts :test #'equalp :key #'car)
    (cdr it)))
  

;; SAMPLES

(defun get-filtered-samples (file filters)
  (let ((tok-samples (get-tokenized-samples file)))
    (if filters
	(select-if (f_ (member _ filters)) tok-samples :key #'cdr)
	tok-samples)))

(defun get-tokenized-samples (file)
  (mapcar (f_ (cons (extract-words (nth-value 2 (tokenize-string (car _)))) (cdr _))) 
	  (read-samples file)))

(defun read-samples (file)
  (with-open-file (stream file)
    (let ((samples nil))
      (ignore-errors
	(loop do (push (cons (read stream) (read stream)) samples)))
      samples)))

;; CLASSIFIERS

(defparameter *tweet-classifier* nil)
(defparameter *tweet-feature-space* nil)

(defun compass-classifier ()
  (aif-ret *tweet-classifier* 
    (setf *tweet-classifier* 
	  (make-instance 'naive-bayes))))

(defun compass-feature-space ()
  (aif-ret *tweet-feature-space* 
    (setf *tweet-feature-space*
	  (make-instance 'feature-space :dimensions 1))))

(defun reset-compass ()
  (setf *tweet-feature-space* nil
	*tweet-classifier* nil))

(defun evaluate-compass-classifier (&key (filename *tweet-input-file*) (classes '(high medium no)))
  (reset-compass)
  (let* ((space (compass-feature-space))
	 (samples (get-filtered-samples filename classes))
	 (features (mapcar (lambda (sample)
			     (cons (let ((class (cdr sample)))
				     (cond ((eq class 'spam) 'spam)
					   ((eq class 'no) 'no)
					   (t 'yes)))
				   (mapcar (curry 'get-feature-vector-id space) 
					   (mapcar #'string-downcase (car sample)))))
			   samples))
	 (training (random-subset features 0.8)) 
	           ;;(subseq features 10))
         (testing (set-difference features training :test #'equalp :key #'cdr)))
    (train-compass-classifier training)
    (let ((results (test-compass-classifier testing)))
      (values results (coerce (/ (1+ (count-if (f_ (eq (car _) (cdr _))) results)) (1+ (length results))) 'float)))))

(defun train-compass-classifier (training-set &key (classifier (compass-classifier)))
  (loop for (class . features) in training-set do
       (train-classifier classifier features class)))

(defun test-compass-classifier (testing-set &key (classifier (compass-classifier)))
  (loop for (class . features) in testing-set collect
       (cons class (predict-class classifier features))))

;; GRAHAM CLASSIFIER

(defun compass-test2 (&optional filters)
  (clear-database)
  (let* ((samples (get-filtered-samples *tweet-input-file* filters))
	 (features (mapcar (lambda (sample)
			     (cons (concat-words 
				    (mapcar #'string-downcase (car sample)))
				   (let ((class (cdr sample)))
				     (cond ((eq class 'no) 'spam)
					   (t 'ham)))))
			   samples))
	 (training (random-subset features 0.8))
	 (testing (set-difference features training :test #'equalp :key #'car)))
    (print (length training))
    (print (length testing))
    (mapc 
     (f_ (train (car _) (cdr _)))
     training)
    (let ((results (mapcar (lambda (test)
			     (cons (cdr test) (classify (car test))))
			   testing)))
      (values results (coerce (/ (count-if (f_ (eq (car _) (cdr _))) results) 
				 (length results))
		      'float)))))

;;; ============================================

;;
;; Tweet DB
;;

(defparameter *tweet-db* (make-hash-table))
(defparameter *tweet-db-tdf* (make-hash-table :test #'equal))

(defun import-tweet-csv-file (filename)
  (with-open-file (stream filename)
    (read-line stream)
    (loop 
       for line = (read-line stream nil nil)
       while line
       do (import-tweet-csv line))
    (compute-db-tdfs)))

;;(defun remove-duplicate-tweets ()
;;  (let ((new (make-hash-table)))
;;    (maphash 

(defun compute-db-tdfs ()
  (setf *tweet-db-tdf* 
	(compute-tdfs 
	 (mapcar (f (item) (caddr item))
		 (hash-items *tweet-db*))))
  (let ((total (hash-table-count *tweet-db*)))
    (maphash (f (term count)
	       (setf (gethash term *tweet-db-tdf*)
		     (/ count total)))
	     *tweet-db-tdf*)))
	   

(defun compute-tdfs (feature-sets)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for set in feature-sets
	 do (mapc (f (term) (incf-hash term hash)) 
		  (remove-duplicates set :test #'equal)))
    hash))

(defun import-tweet-csv (string)
  (let ((list (csv->list string)))
    (when (> (length list) 1)
      (dbind (idst tweet) list
	(setf (gethash (parse-integer idst) *tweet-db*)
	      (list tweet (tweet-features tweet))))))) ;; (equal intent "Y")))))))

(defun tweet-features (tweet)
  (remove-if (f (term)
	       (or (stopword? (id-for-token term))
		   (< (length term) 2)))
	     (extract-words 
	      (mvretn 3 (langutils::tokenize-string 
			 (string-downcase tweet))))))

;;
;; Search DB
;;

(defun search-tweet-db (query &key (max nil) (features nil))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
	   (fixnum max) (boolean features))
  (labels ((tweet-term-match (features terms)
	     (declare (list features terms))
	     (every (f (term) (member term features :test #'equal)) terms)))
    (declare (dynamic-extent (function tweet-term-match)))
    (let ((results nil)
	  (terms (tweet-features query))
	  (count 0))
      (declare (dynamic-extent terms count)
	       (list terms results)
	       (fixnum count))
      (time (maphash (f (id rec)
		 (declare (ignore id)
			  (fixnum id) (list rec))
		 (when (tweet-term-match (second rec) terms)
		   (push (if features (second rec) (first rec))
			 results)
		   (when (and max (< max (incf count)))
		     (return-from search-tweet-db results))))
	       *tweet-db*))
      results)))


;;
;; Tag cloud 2 
;;

(defun compute-tag-cloud (query &key (max 100) (tags 30))
  (declare (optimize speed) (safety 0) (debug 0))
  (let* ((qterms (tweet-features query))
	 (qdocs (mapcan (f (term) (search-tweet-db term :max max :features t)) qterms))
	 (chash (make-hash-table :test #'equal)))
    (declare (list qterms qdocs)
	     (hash-table chash))
    (mapc (f (terms) 
	    (declare (list terms))
	    (dolist (term terms) ;;(remove-duplicates terms :test #'equal)) 
	      (incf-hash term chash)))
	  qdocs)
;;    (normalize-terms chash *tweet-db-tdf*)
    (top-n (remove-if (f (item) (member (car item) qterms :test #'equal))
		      (sort (hash-items chash) #'> :key #'cdr) )
	   tags)))

(defun normalize-terms (chash tfhash)
  (maphash (f (term count) 
	     (setf (gethash term chash) 
		   (/ count (gethash term tfhash))))
	   chash))
	   


;;
;; Tag cloud 1
;;

(defun compute-tag-cloud-v1 (query)
  (let ((qterms (tweet-features query))
	(tweets (sorted-tweets (search-tweet-db query t)))
	(chash (make-hash-table :test #'equal)))
    (mapc (curry2 #'update-tag-counts chash) tweets)
    (top-n (sort (gethash (first qterms) chash)
		 #'> :key #'cdr)
	   20)))

(defun sorted-tweets (tweets)
  (sort (loop for tweet in tweets
	   collect (sort (copy-list tweet) #'string<))
	#'string<
	:key #'first))

(defun update-tag-counts (tweet hash)
  (labels ((rec (counts cterm terms)
	     (when terms
	       (loop for term in terms do
		    (setf counts (assoc-incf term counts)))
	       (setf (gethash cterm hash) counts)
	       (rec (gethash (first terms) hash) (first terms) (rest terms)))))
    (rec (gethash (first tweet) hash) (first tweet) (rest tweet))))
       

(defun assoc-incf (elt alist)
  (aif (assoc elt alist :test #'equal)
       (progn (incf (cdr it)) alist)
       (apush elt 1 alist)))

    



