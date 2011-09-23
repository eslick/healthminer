(in-package :smart)

;; Process:
;; Given CRF fields
;; Find term pairs
;; Extract features
;; Label with type (if known or negative)
;; Train classifier (EM)

;; Problems:
;; - Feature extraction
;; - UMLS semantic types (supervised set)
;; - binary vs. nominal set

;; EM Process for NB labeling of sentence context
;; 1) Label P = c+, U = c-
;; 2) Train classifier
;; 3) Apply classifier to U
;; 4) Label P = c+, U = classifier result
;; 5) Return to step #2
;;
;; Iterate until no change in labeling is < epsilon

;; Extract pairs + types
;; Manually evaluate results


;;
;; EM Process
;;

(defun run-relational-em (labeled unlabeled)
  "Take labeled set and perform EM"
  (let ((auto-labeled unlabeled))
    (loop 
       do 
	 (train-labeled-rel-windows (append labeled unlabeled)) ;; maximization
	 (let ((new (classify-rel-windows (cdrs unlabeled)))) ;; compute expectation
	   (if (equal (cars auto-labeled) (cars new)) ;; if converged
	       (return new)                           ;; return
	       (setf auto-labeled new))))))           ;; use new label set

;;
;; Training and classification
;;

(defun train-labeled-rel-windows (lwindows &key (classifier (rel-classifier)))
  "Train the classifier according to "
  (loop for (class . window) in lwindows do
       (train-classifier classifier (window-features window) class)))
	
(defun classify-rel-windows (windows &optional (classifier (rel-classifier)))
  (mapcar (f_ (cons (predict-class classifier (window-features _)) _)) windows))

;;
;; UMLS labels for training set
;;

(defun umls-label-windows (windows)
  (mapcar #'umls-label-window windows))


;;
;; Getting training set
;;

(defparameter *window-size* 40)
(defparameter *window-margin* 5)

(defun get-relation-windows (crf-hash)
  "Returns (term1 type1 term2 type2 id-list message)"
  (let ((pairs (all-crf-pairs crf-hash)))
    (mapcar #'get-relation-window pairs)))

(defun get-relation-window (crf-pair crf-hash)
  "Given a pair of crf-fields get the window between them with margin 
   using appropriate parameters"
  (dbind (left . right) crf-pair
    (awhen (crf-pair-window left right crf-hash)
      (list left (type left) right (type right) it 
	    (crf-field-message left)))))

(defun crf-pair-window (left right crf-hash)
  "Given two fields from a message, find the terms including and between them"
  (let* ((left-range (crf-field-message-range left crf-hash))
	 (right-range (crf-field-message-range right crf-hash))
	 (words (message-words (crf-field-message left))))
    (when (< (- (first right-range) (second left-range)) *window-size*)
      (safe-subseq words
		   (- (first left-range) *window-margin*)
		   (+ (second right-range) *window-margin*)))))

;;
;; UMLS integration
;;

(defun get-umls-relation-labels (windows)
  "Prepends label to windows or nil if no label"
  (mapcar (f_ (cons nil _)) windows))

;;
;; Utilities
;;

(defparameter *rel-feature-space* nil)
(defparameter *rel-classifier* nil)

(defun rel-feature-space ()
  (retset *rel-feature-space* 
	  (make-instance 'feature-space :dimensions 1)))

(defun rel-classifier (&optional reset)
  (if reset
      (setf *rel-classifier* (make-instance 'naive-bayes))
      (retset *rel-classifier* (make-instance 'naive-bayes))))

(defun window-features (window &key (f-space (rel-feature-space)))
  (let ((order (strcat (second window) "|" (fourth window))))
    (mapcar (curry #'get-feature-vector-id f-space)
	    (cons order (mapcar #'get-lemma-for-id (fifth window))))))
    
	


