(in-package :smart)

;; =============================================
;; LDA++
;; =============================================

;;
;; Write LDA training file
;;

(defparameter *feature-space* nil)

(defun dump-for-lda (messages doc-file vocab-file &optional fspace)
  "Create a file for input to lda-c"
  (let ((*feature-space* (or fspace (make-instance 'feature-space :dimensions 1))))
    (with-output-file (stream doc-file)
      (loop for message in messages do
	   (dump-message-for-lda message stream)))
    (dump-lda-vocab *feature-space* vocab-file)
    *feature-space*))

(defun dump-message-for-lda (message stream)
  "Dump a messages as a simple word distribution"
  (multiple-value-bind (uniques pairs)
      (compute-lda-wdist (document-text (message-doc message)))
    (format stream "~A ~{~{~A:~A ~}~}~%" uniques pairs)))

(defun compute-lda-wdist (vector)
  "Compute the lda distribution statistic for a token vector"
  (let ((counts (vector-counts vector)))
    (values (length counts) counts)))
  
(defun vector-counts (vector)
  "Return a list containing the (element count) for each unique value in vector"
  (let ((hash (make-hash-table :size (round (* 1.4 (length vector))) :test #'eq))
	(counts nil))
    (loop for elt across vector do
	 (incf-hash (get-feature-vector-id *feature-space* elt) hash))
    (maphash (f (k v) (push (list k v) counts))
	     hash)
    counts))

;;
;; Write vocab file
;;

(defun dump-lda-vocab (fspace file)
  (with-output-file (str file)
    (loop for i from 1 upto (feature-space-index fspace) do
	 (format str "~A~%" (token-for-id (first (get-feature-vector fspace i)))))))


;;
;; Variational inference
;;

(defun do-topic-assignment (model messages name fspace)
  (dump-for-lda messages "~/temp"))


;;
;; Load document assignments
;;

(defun load-topic-assignment (messages file vocab)
  (when (stringp vocab)
    (setf vocab (read-lda-vocab-to-fspace vocab)))
  (with-open-file (str file)
    (loop 
       for line = (read-line str nil)
       for message = (and messages (pop messages))
       while line
       collect (cons message (read-lda-topic-line line vocab)))))

(defun read-lda-vocab-to-fspace (file)
  (let ((fspace (make-instance 'feature-space :dimensions 1 :index 0)))
    (with-open-file (str file)
      (loop 
	 for line = (read-line str nil)
	 while line
	 do (get-feature-vector-id fspace (id-for-token line))))
    fspace))

(defun read-lda-topic-line (string fspace)
  "Turn line ''495 123:12 678:1 ...'' into '((123 12) (678 1) ...)"
  (loop 
     for entry in (rest (split-sequence:split-sequence #\Space string)) 
     for values = (mapcar #'parse-integer  
			  (split-sequence:split-sequence #\: entry))
     collect
       (cons (aif (first (get-feature-vector fspace (first values)))
		  (token-for-id it)
		  " ")
	     (second values))))

;;
;; 
;;