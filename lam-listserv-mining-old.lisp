(in-package :smart)

;; =====================================================================
;;  Miscellaneous Calculations
;; =====================================================================

;;
;; TF / IDF
;;

(defvar *df* (make-hash-table))
(defvar *tf* (make-hash-table))
(defvar *tf-fast* (make-hash-table))

;; (defun compute-fast-tf ()
;;   (let* ((terms (round (* 1.2 (hash-table-count *word-counts*))))
;; 	 (msgs (+ (hash-table-count *message-map*) 10)))
;; 	(docmap (make-hash-table :size (hash-table-count *message-map*)
;; 	(array 
;; 	 (make-array (list (hash-table-count *word-counts*)
;; 			   (hash-table-count *message-map*))
;; 		     :element-type 'single-float)))
;;     (setf *tf-fast* array)
;;     (map-messages 
  

(defun compute-tf (message)
  (let ((tf-hash (make-hash-table)))
    (setf (gethash message *tf*) tf-hash)
    (count-words (body message) tf-hash)
    (let ((norm (hash-sum tf-hash)))
      (maphash (lambda (term count)
		 (setf (gethash term tf-hash)
		       (/ count norm)))
	       tf-hash))
    tf-hash))
	       

(defun hash-sum (hash &aux (sum 0))
  (maphash (lambda (k v)
	     (declare (ignore k))
	     (incf sum v))
	   hash)
  sum)

(defun compute-all-tf ()
  (map-messages (id rec)
    (compute-tf (get-object id))))

;; IDF

(defun compute-idf (term total-docs &aux (docs 0))
  (maphash (lambda (msg tfs)
	     (declare (ignore msg))
	     (when (gethash term tfs) (incf docs)))
	   *tf*)
  (setf (gethash term *df*)
	(log (/ total-docs (1+ docs)))))

(defun compute-idfs ()
  (let ((total-docs (length (get-instances-by-class 'message))))
    (maphash (lambda (msg tf-hash)
	       (declare (ignore msg))
	       (maphash (lambda (term freq)
			  (declare (ignore freq))
			  (unless (gethash term *df*)
			    (compute-idf term total-docs)))
			tf-hash))
	     *tf*)))

(defun compute-tf/idf-tables ()
  (compute-all-tf)
  (compute-idfs))

;;
;; Key terms for LSA probes
;;

(defparameter *unary-symptoms* 
  '("breathing" "fatigue" "period" "cycle" "chyle" "chest"
    "pain" "stress" "sleep" "cough" "sick" "die" "liquid"
    "heal" "pneumothorax"))

(defparameter *influences*
  '("rapo" "rapamyacin" "sleep" 
    "lam" "prayer" "meditation" 
    "oxygen" "stress" "medication"))

(defparameter *related*
  '("hospital" "husband" "disease" "surgery"
    "diagnose" "diagnosis" "oximeter" "spirometer"))

(defparameter *all-related*
  (append *unary-symptoms* *influences* *related*))

;;
;; Causative verb extraction (ala Wolff, Song and Driscoll '02)
;;
(defparameter *causal-verbs*
  (mapcar #'string-downcase 
	  (mapcar #'symbol-name 
		  '(cause because causes make let help get prevent
		    enable force hinder hold impede keep 
		    make makes permit protect save set start stop))))

;;(defparameter *causal-verb-tokens*
;;  (mapcar #'id-for-token *causal-verbs*))

(defparameter *opinion-verbs*
  '(believe think guess feel))
    
(defun causal-verb-counts ()
  (loop for verb in *causal-verbs* collect
       (cons verb (gethash (symbol-name verb) *word-counts*))))

(defun extract-causal-verb-windows (&optional (wsize 30))
  (loop for verb in *causal-verbs* collect
       (extract-causal-verb-window verb size)))

(defun extract-causal-verb-window (verb size)
  (let ((windows nil))
    (maphash (lambda (id words)
	       (declare (ignore id))
	       (let ((positions (term-positions verb words)))
		 (when positions
		   (setf windows
			 (append windows 
				 (windows-for-positions size positions words))))))
	     (get-message-map))
    windows))

