;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-

(in-package :smart)

;; --------------------------------------------------
;; Implementation of a Naive Bayes Classifier
;; --------------------------------------------------

(defclass naive-bayes ()
  ((vector-alist :accessor naive-bayes-alist :initform nil :initarg :vector)
   (vector-hint :accessor naive-bayes-vector-hint :initform 100 :initarg :vector-hint)
   (index-fn :accessor naive-bayes-index-fn :initform #'identity :initarg :index-fn)
   (samples :accessor naive-bayes-samples :initform nil :initarg :samples)
   (inspect :accessor naive-bayes-inspect-p :initform t :initarg :inspect-p )
   (feature-dict :accessor naive-bayes-feature-dict :initform (make-hash-table) :initarg :feature-dict))
  (:documentation "A naive bayes classifier for arbitrary feature types"))

(defun make-naive-bayes-classifier (&key index-fn (hint 100) (inspect t))
  (make-instance 'naive-bayes
		 :vector-hint hint
		 :index-fn (if index-fn index-fn (make-hashed-index-fn hint))
		 :inspect-p inspect
		 :feature-dict (when inspect (make-hash-table))))

  
(defun make-hashed-index-fn (&optional (hint 100) &key (test 'equal))
  (let ((offset-hash (make-hash-table :size hint :test test))
	(next-idx -1))
    (labels ((get-index (feature)
			(acond ((null feature)
				(1- next-idx))
			       ((gethash feature offset-hash)
				it)
			       (t (setf (gethash feature offset-hash)
					(incf next-idx))))))
      #'get-index)))

(defmethod top-features ((c naive-bayes) class count &optional (convert 'token-for-id))
  (let* ((vector (cdr (assoc class (naive-bayes-alist c))))
	 (counts (array->list vector))
	 (offsets (map0-n #'identity (1- (length counts))))
	 (feature-dict (naive-bayes-feature-dict c))
	 (order (sort-b-according-to-a counts offsets #'>)))
    (mapc (lambda (index)
	      (format t "index: ~A  feature: ~A~%" index 
		      (funcall convert (gethash index feature-dict))))
	  (subseq order 0 count))))

(defmethod most-significant-features ((c naive-bayes) class not-class count 
				      &optional (convert 'token-for-id))
   (let* ((class-vector (cdr (assoc class (naive-bayes-alist c))))
	  (not-class-vector (cdr (assoc not-class (naive-bayes-alist c))))
	  (log-ratio (mapcar (lambda (cl ncl)
			       (if (eq ncl 0)
				   (- (log (* (length class-vector) 10)))
				   (log (/ (/ (+ cl 1.0d0) (get-class-samples c class))
					   (/ (+ ncl 1.0d0) (get-class-samples c not-class))))))
			     (remove-nulls (array->list class-vector))
			     (remove-nulls (array->list not-class-vector))))
	  (offsets (map0-n #'identity (1- (length log-ratio))))
	  (order (sort-b-according-to-a log-ratio offsets #'>))
	  (feature-dict (naive-bayes-feature-dict c)))
     (mapc (lambda (index)
	     (format t "index: ~A  feature: ~A~%" index 
		     (let ((feature (gethash index feature-dict)))
		       (etypecase feature
			 (list (list (funcall convert (car feature)) (cdr feature)))
			 (fixnum (funcall convert feature))))))
	   (subseq order 0 count))
     nil))

(defmethod store-classifier-in-root (root-name classifier)
  "Store a classifier when it has a feature dict"
  (assert (> (hash-table-size (naive-bayes-feature-dict classifier)) 0))
  (add-to-miner-root root-name
		     (list (naive-bayes-alist classifier)
			   (naive-bayes-samples classifier)
			   (naive-bayes-feature-dict classifier)))
  t)

(defmethod restore-classifier-from-root (root-name)
  (let ((list (get-from-miner-root root-name)))
    (assert (= (length list) 3))
    (assert (hash-table-p (third list)))
    (dbind (alist samples feature-dict) list
      (let ((c (make-naive-bayes-classifier :hint (hash-table-size feature-dict))))
	(setf (naive-bayes-samples c) samples)
	(setf (naive-bayes-alist c) alist)
	(setf (naive-bayes-feature-dict c) feature-dict)
	(let ((sorted-index+features (sort (hash-items feature-dict) #'< :key #'car)))
	  (dolist (pair sorted-index+features)
	    (let ((new-idx (funcall (naive-bayes-index-fn c) (cdr pair))))
	      (assert (eq new-idx (car pair))))))
	c))))
	   

;; 
;; Basic methods...
;;

(defmethod reset-classifier ((c naive-bayes))
  (setf (naive-bayes-alist c) nil
	(naive-bayes-samples c) nil))

(defmethod add-class ((c naive-bayes) class)
  (push (cons class (make-array (naive-bayes-vector-hint c)
				:element-type 'fixnum 
				:initial-element 0
				:adjustable t))
	(naive-bayes-alist c))
  (push (cons class 0) (naive-bayes-samples c)))
	
(defmethod naive-bayes-classes ((c naive-bayes))
  (mapcar #'car (naive-bayes-samples c)))


;;
;; Training....
;;

(defmethod train-classifier ((c naive-bayes) features class &rest opts &key (class-test #'eq) &allow-other-keys)
  "Count each occurance of feature for the class, and total occurances.  A null
   class argument will just count baseline samples.  The samples variable is the
   number of provided vectors"
  (assert (listp features))
  (flet ((get-index (elt entry)
	   (let ((index (get-training-index c elt)))
	     (when (>= index (length (cdr entry)))
	       (setf (cdr entry) (adjust-array (cdr entry) (floor (* 1.5 index)) :initial-element 0)))
	     index)))
    (acond ((null class)
	    (count-class-sample c class :class-test class-test))
	   ((assoc class (naive-bayes-alist c) :test class-test)
	    (assert (consp it))
	    (dolist (elt features)
;;	      (format t "elt: ~A  it: ~A  index: ~A~%" elt it (get-index elt it))
	      (incf (aref (cdr it) (get-index elt it))))
	    (count-class-sample c class :class-test class-test))
	   (t (add-class c class)
	      (train-classifier c features class)))))

(defmethod get-training-index ((c naive-bayes) feature)
  (let ((index (funcall (naive-bayes-index-fn c) feature)))
    (when (naive-bayes-inspect-p c)
      (unless (gethash index (naive-bayes-feature-dict c))
	(setf (gethash index (naive-bayes-feature-dict c)) feature)))
    index))

(defmethod count-class-sample ((c naive-bayes) class &key class-test)
  (incf (cdr (assoc class (naive-bayes-samples c) :test class-test))))

(defmethod get-class-samples ((c naive-bayes) class)
  (cdr (assoc class (naive-bayes-samples c))))

(defmethod get-all-samples ((c naive-bayes))
  (sum-list (cdrs (naive-bayes-samples c))))

;; 
;; Prediction...
;; 

(defmethod predict-class ((c naive-bayes) features &rest opts)
  (declare (ignore opts))
  (assert (listp features))
  (let* ((probabilities (mapcar (lambda (class) (cons class (probability-of-class c class features)))
				(naive-bayes-classes c)))
	 (winning-class (car (best (lambda (c1 c2) (> (cdr c1) (cdr c2))) 
				   probabilities))))
    (values winning-class probabilities)))

(defmethod probability-of-class ((c naive-bayes) class features)
  (let ((vector (cdr (assoc class (naive-bayes-alist c))))
	(pos-features (sort (mapcar (naive-bayes-index-fn c) features) #'<))
	(class-samples (get-class-samples c class))
	(total-samples (get-all-samples c))
	(product 1))
    (loop for index in pos-features do
	 (progn 
	   (when (>= index (length vector))
	     (setf (cdr (assoc class (naive-bayes-alist c)))
		   (adjust-array vector
				 (floor (* 1.5 (length vector)))
				 :initial-element 0)))
	   (setf product
		 (* product 
		    (/ (+ (aref vector index) 1.0d0)
		       (+ class-samples 1.0d0))))))
    (* product (/ class-samples total-samples))))

;;
;; Prediction over alternatives...
;;

(defmethod predict-binary-class ((c naive-bayes) class not-class features)
  (let ((class-vector (cdr (assoc class (naive-bayes-alist c))))
	(not-class-vector (cdr (assoc not-class (naive-bayes-alist c))))
	(feature-indexes (mapcar (naive-bayes-index-fn c) features))
	(class-samples (get-class-samples c class))
	(not-class-samples (get-class-samples c not-class))
	(total-samples (get-all-samples c)))
    (let ((sum (log (/ (/ class-samples total-samples)
		       (/ not-class-samples total-samples)))))
      (loop for index in feature-indexes do
	   (when (and (< index (length class-vector))
		      (< index (length not-class-vector)))
	     (let ((prob-feature-with-class
		    (/ (+ (aref class-vector index) 1.0d0)
		       (+ class-samples 1.0d0)))
		   (prob-feature-without-class
		    (/ (+ (aref not-class-vector index) 1.0d0)
		       (+ not-class-samples 1.0d0))))
	       (incf sum (log (/ prob-feature-with-class
				 prob-feature-without-class))))))
      (values (> sum 0) sum))))


;;
;; Example & Test
;; 	       
     
;; tennis? . features
(defparameter *naive-bayes-test-data*
  '((:noplay . (:outlook-sunny :temp-hot :humidity-high :wind-weak))
    (:noplay . (:outlook-sunny :temp-hot :humidity-high :wind-strong))
    (:play . (:outlook-overcast :temp-hot :humidity-high :wind-weak))
    (:play . (:outlook-rain :temp-mild :humidity-high :wind-strong))
    (:play . (:outlook-rain :temp-cool :humidity-normal :wind-weak))
    (:noplay . (:outlook-rain :temp-cool :humidity-normal :wind-strong))
    (:play . (:outlook-overcast :temp-cool :humidity-normal :wind-strong))
    (:noplay . (:outlook-sunny :temp-mild :humidity-high :wind-weak))
    (:play . (:outlook-sunny :temp-cool :humidity-normal :wind-weak))
    (:play . (:outlook-rain :temp-mild :humidity-normal :wind-weak))
    (:play . (:outlook-sunny :temp-mild :humidity-normal :wind-strong))
    (:play . (:outlook-overcast :temp-mild :humidity-high :wind-strong))
    (:play . (:outlook-overcast :temp-hot :humidity-normal :wind-weak))
    (:noplay . (:outlook-rain :temp-mild :humidity-high :wind-strong))))

(defun run-test-query ()
  (let ((nbc (make-naive-bayes-classifier :hint 12)))
    (mapc (lambda (entry) 
	    (train-classifier nbc (cdr entry) (car entry)))
	  *naive-bayes-test-data*)
    (let ((prediction (predict-class nbc '(:outlook-sunny :temp-cool :humidity-high :wind-strong))))
      (format t "Prediction: ~A.  Is it correct? ~A~%" prediction (eq prediction :noplay)))))
			       
  




