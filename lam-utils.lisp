(in-package :smart)

;; ==========================================================
;; Stopwords
;; ==========================================================

(defvar *stopwords* nil)

(defun ensure-stopword-hash ()
  (unless *stopwords*
    (setf *stopwords* (get-from-root '*stopwords*)))
  (unless *stopwords*
    (setf *stopwords* (make-hash-table :size 1000 :test 'equal ))))

(defun import-stopwords (file)
  (ensure-stopword-hash)
  (unless file (setf file "/Users/eslick/Work/fsrc/lisp/langutils/data/concise-stopwords.txt"))
  (with-open-file (stream file)
    (do-stream-lines (line stream)
      (setf (gethash (normalize-string line) *stopwords*) t)))
  (add-to-root '*stopwords* *stopwords*))

(defun stopword-p (word)
  (gethash word *stopwords*))

;; ===========================================================
;; Phrase Manipulation
;; ===========================================================


(defun term-positions (word ids)
  (let ((position 0)
	(positions nil)
	(id (id-for-token word)))
    (loop 
       (let ((pos (position id ids :start position)))
	 (if (null pos)
	     (return-from term-positions (nreverse positions))
	     (progn
	       (push pos positions)
	       (setf position (1+ pos))))))))

(defun phrase-windows-around-word (word window-size msg-ref)
  "Given a list of positions, extract words around"
  (let ((terms (message-lemmas msg-ref)))
    (loop for term-position in (term-positions word terms)
	 collect 
	 (position-window (position-if (curry 'phrase-contains-point term-position)
				       (message-chunks msg-ref))
			  window-size (message-chunks msg-ref)))))

(defun position-window (position size terms)
  (when (and position terms)
    (let* ((length (length terms))
	   (interval (round size 2))
	   (start (max (- position interval) 0))
	   (end (min (+ position interval 1) length)))
      (values (subseq terms start end) start end))))

(defun phrase-contains-point (point phrase)
  (and (>= point (phrase-start phrase) )
       (<= point (phrase-end phrase))))

(defun term-phrase-windows (term size)
  (let (windows)
    (map-messages (id rec)
      (awhen (phrase-windows-around-word term size id) 
	(push it windows)))
    (flatten1 windows)))

(defun term-lexical-windows (term size)
  (let ((windows nil)
	(lterm (id-for-token term)))
    (map-messages (id rec)
      (push (mapcar (lambda (position)
		      (cons id (tokens-for-ids
				(position-window position size (mmap-words rec)))))
		    (term-positions lterm (mmap-words rec)))
	    windows))
    (flatten1 windows)))

(defun review-term-lexical-windows (term size &optional count)
  (more (mapcar 'concat-words (if count (subseq #1=(term-lexical-windows term size) 0 count)
				  #1#))))
		

;; ===========================================================
;;  Shorthand
;; ===========================================================

(defmacro with-output-file ((var path) &body body)
  `(with-open-file (,var ,path
			 :direction :output
			 :if-exists :supersede)
     ,@body))

;; ===========================================================
;; Manual classification
;; ===========================================================

(defun random-label-dataset (dataset samples &key description subset (key #'identity))
  (let* ((raw-list (if (eq (type-of dataset) 'dataset)
		       (funcall key (dataset-data dataset))
		       dataset))
	 (list (if subset (subseq raw-list 0 subset) raw-list))
	 (random-subset (random-subset list samples))
	 (results (loop for elt in random-subset
		     collect (progn
			       (format t "Label for '~A': " elt)
			       (cons elt (read))))))
    (make-instance 'evaluation :description description
		   :dataset (if (listp dataset) "List argument" dataset)
		   :results results)))

(defun extract-term-list (data)
  (typecase data
    (hash-table (cars (hash-items data)))
    (list (if (consp (second data))
	      (second data)
	      data))))
   
(defun label-random-element (list len)
  (let ((elt (nth (random len) list)))
    (format t "Label for '~A': " elt)
    (cons elt (read))))

(defun convert-rel-labels (labels)
  (mapcar #'convert-rel-label labels))

(defun convert-rel-label (label)
  (case label
    (s 'scientific) ;; Something you could read or get from a dr. / may not be accurate
    (p 'patient) ;; An interesting patient or lifestyle proposal / feasible
    (a 'anaphor) ;; makes sense, but obscured by anaphor
    (o 'other)   ;; General conversation or too specific
    (j 'junk)))  ;; Phrases are junk or too general

(defun convert-fragment-labels (labels)
  (mapcar #'convert-label labels))

(defun convert-fragment-label (label)
  (case label
    (d 'domain)
    (s 'specific)
    (r 'relevant)
    (g 'general)
    (j 'junk)))

(defun histogram (list &key (test #'equalp) key)
  (let ((table (make-hash-table :test test)))
    (mapcar (lambda (elt) 
	      (when key (setf elt (funcall key elt)))
	      (incf-hash elt table)) list)
    (sort (hash-items table) #'> :key #'cdr)))

(defun print-evaluation (evaluation &optional (label-converter 'convert-fragment-labels))
  (when (> (length (evaluation-results evaluation)) 2)
    (unless (stringp (evaluation-dataset evaluation))
      (format t "Dataset: ~A~%  ~A~%" 
	      (dataset-title (evaluation-dataset evaluation))
	      (dataset-description (evaluation-dataset evaluation))))
    (format t "Evaluation: ~A~%(e.g ~A)" 
	    (evaluation-description evaluation)
	    (subseq (evaluation-results evaluation) 0 2))
    (let* ((results (evaluation-results evaluation))
	   (hist (histogram 
		  (funcall label-converter
			   (mapcar #'cdr results)))))
      (print hist)
      (format t "~%Summary: ~A% relevant~%~%" 
	      (coerce
	       (cond ((assoc 'domain hist)
		      (* 100 
			 (/ (+ (cdr (assoc 'domain hist))
			       (cdr (assoc 'relevant hist)))
			    (length (evaluation-results evaluation)))))
		     ((assoc 'scientific hist)
		      (* 100
			 (/ (+ (cdr (assoc 'scientific hist))
			       (cdr (assoc 'patient hist))
			       (cdr (assoc 'anaphor hist)))
			    (length (evaluation-results evaluation)))))
		     (t 0))
	       'float)))
    nil))

  
;;
;; Small morsels
;;

(defun concat-words (strings)
  (when strings ;; (every #'(lambda (x) (> (length x) 0)) strings))
    (merge-or-regex-strings strings #\Space)))

(defmacro seti (var expr)
	 `(progn (setf ,var ,expr) t))

(defun more (list &key (size 10) (fn #'identity) (print t))
  (loop for elt in list 
     for i from 1 do
     (progn 
       (let ((value (funcall fn elt)))
	 (cond ((symbolp print) (funcall print value))
	       ((null print) nil)
	       (t (print value))))
       (when (= (mod i size) 0)
	 (when (eq (read-char) #\q)
	   (return nil))))))

;;
;; Web hit counts
;;

