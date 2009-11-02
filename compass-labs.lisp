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


