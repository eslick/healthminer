(defpackage :umls (:use :cl :stdutils :f-underscore))

(in-package :umls)


;;
;; Trivial UMLS Semantic Network
;;

(defstruct umls-type 
  rtp id name abr def tree inv)

(defstruct umls-relation
  source rel target)

(defmethod print-object ((relation umls-relation) stream)
  (format stream "#<UMLS-REL ~A '~A' -> '~A'>"
	  (umls-semantic-get (umls-relation-rel relation) :name)
	  (umls-semantic-get (umls-relation-source relation) :name)
	  (umls-semantic-get (umls-relation-target relation) :name)))

(defun print-umls-relation (relation &optional (stream t))
  (format stream "(~A '~A' -> '~A')"
	  (umls-semantic-get (umls-relation-rel relation) :name)
	  (umls-semantic-get (umls-relation-source relation) :name)
	  (umls-semantic-get (umls-relation-target relation) :name)))

(defvar *umls-semantic-types* (make-hash-table :test #'eq))
(defvar *umls-semantic-rels* (make-hash-table :test #'eq))

(defun load-umls-semantic-net (directory)
  (setf *umls-semantic-types* (make-hash-table :test #'eq))
  (setf *umls-semantic-rels* (make-hash-table :test #'eq))
  ;; Types
  (with-open-file (stream (merge-pathnames (make-pathname :name "SRDEF" :type "txt")
					   (make-pathname :directory directory)))
    (do-contentful-lines (line stream)
      (dbind (rt tid name tree desc ex un nh abr rin misc)
	  (split-sequence:split-sequence #\| line)
	(declare (ignore ex un nh misc))
	(let ((id (intern tid)))
	  (setf (gethash id *umls-semantic-types*)
		(make-umls-type :rtp (intern rt)
				:id id
				:name name
				:abr abr
				:def desc
				:tree tree
				:inv rin))))))
  ;; Relations
  (with-open-file (stream (merge-pathnames (make-pathname :name "SRSTRE1" :type "html")
					   (make-pathname :directory directory)))
    (do-contentful-lines (line stream)
      (dbind (sid rid tid misc)
	  (split-sequence:split-sequence #\| line)
	(declare (ignore misc))
	(let ((source (intern sid))
	      (relation (intern rid))
	      (target (intern tid)))
	  (hash-push *umls-semantic-rels*
		     relation
		     (make-umls-relation
		      :source source
		      :rel relation
		      :target target)))))))

;;
;; UMLS Semantic Net API
;;

(defun umls-semantic-get (ref &optional field abbrev)
  (let ((rec (find ref (hash-values *umls-semantic-types*)
		   :key (typecase ref
			  (string (if abbrev
				      #'umls-type-abr
				      #'umls-type-name))
			  (symbol #'umls-type-id))
		   :test #'equalp)))
    (when rec (typecase field
		(null rec)
		(symbol (slot-value 
			 rec 
			 (if (keywordp field)
			     (intern (symbol-name field) (find-package :umls))
			     field)))))))

(defun umls-isa-hierarchy (ref)
  (if (null ref) nil
      (let ((rel (umls-get-relation ref "isa")))
	(when rel
	  (cons (umls-semantic-get ref :name)
		(umls-isa-hierarchy (umls-relation-target rel)))))))
   
(defun umls-get-relations-by-source (source type)
  (select-if (umls-semantic-get source :id)
	     (umls-get-relations type)
	     :key #'umls-relation-source))

(defun umls-get-relations-by-target (type target)
  (let ((id  (umls-semantic-get target :id)))
    (select-if (f (targ) (eq id targ))
	       (umls-get-relations type)
	       :key #'umls-relation-target)))

(defun umls-get-types-for (source target)
  (let ((src (umls-semantic-get source :id))
	(dst (umls-semantic-get target :id)))
  (select-if (f (rel)
	       (and (eq (umls-relation-source rel) src)
		    (eq (umls-relation-target rel) dst)))
	     (flatten (hash-values *umls-semantic-rels*)))))

(defun umls-get-relations (type)
  (gethash (umls-semantic-get type :id) *umls-semantic-rels*))

;;
;; Trivial UMLS
;;

;; Representing the entire UMLS database is unreasonable, so 
;; we'll need to pull what we need from the files when we need them.  
;; Fortunately we can do binary searches over files reasonably fast

(defvar *umls-files* nil)

(defun initialize-umls (umls-dir semantic-dir)
  (let* ((umls-path (merge-pathnames (make-pathname :directory umls-dir)))
	 (umls-base (merge-pathnames (make-pathname :directory '(:relative "META"))
				     umls-path))
	 (index-path (merge-pathnames (make-pathname :directory '(:relative "META" "indexes"))
				      umls-path)))
    (unless (file-write-date umls-path)
      (error "UMLS directory provided, ~A, is invalid" umls-path))
    (unless (file-write-date index-path)
      (error "UMLS indexes not found, ~A" index-path))
    (unless (file-write-date index-path)
      (error "UMLS indexes not found, ~A" index-path))
    (setf *umls-files* 
	  `((word-idx . ,(make-umls-path umls-base "MRXW_ENG.RRF"))
	    (norm-word-idx . ,(make-umls-path umls-base "MRXNW_ENG.RRF"))
	    (concept-type . ,(make-umls-path umls-base "MRSTY.RRF"))
	    (concept-detail . ,(make-umls-path umls-base "MRCONSO.RRF"))
	    (concept-def . ,(make-umls-path umls-base "MRDEF.RRF"))))
    (load-umls-semantic-net semantic-dir)))

(defun make-umls-path (base filename)
  (dbind (name type) (split-sequence:split-sequence #\. filename)
    (merge-pathnames base (make-pathname :name name :type type))))

(defun get-umls-file (tag)
  (cdr (assoc tag *umls-files*)))

;;
;; UMLS 
;;

(defparameter *procedures* nil)
(defparameter *symptoms* nil)
(defparameter *condition* nil)

(defun setup-umls-training-ngrams ()
  (unless *procedures*
    (print "Loading procedures")
    (setf *procedures* (umls-procedure-ngrams)))
  (unless *symptoms*
    (print "Loading symptoms")
    (setf *symptoms* (umls-symptom-ngrams)))
  (unless *condition*
    (print "Loading conditions")
    (setf *condition* (umls-condition-ngrams)))
  t)

(defun umls-procedure-ngrams (&optional (type 'smart::treat))
  (umls-type-ngrams   
   type
   "Therapeutic or Preventive Procedure"))

(defun umls-symptom-ngrams (&optional (type 'smart::sympt))
  (umls-type-ngrams 
   type
   "Sign or Symptom"
   "Laboratory or Test Result"))

(defun umls-condition-ngrams (&optional (type 'smart::cond))
  (umls-type-ngrams 
   type
   "Mental or Behavioral Dysfunction" 
   "Disease or Syndrome"))

(defun umls-type-ngrams (label &rest types)
  (concepts->crf-ngrams label
   (mapcan (f (type)
	     (umls-concepts-for-type
	      (umls-semantic-get type :id)))
	   types)))
  

(defun concepts->crf-ngrams (label concepts)
  (collect (curry #'concept->crf-ngram label) concepts))

(defun concept->crf-ngram (label concept)
  (smart::ngram-as-ids 
   (string->crf-ngram label 
    (umls-concept-string concept))))

(defun string->crf-ngram (label string)
  (let ((words (extract-words 
		(mvretn 3 (langutils:tokenize-string 
			      (string-downcase string))))))
    (when (<= (length words) 3)
      (append words (list label)))))

;;
;; UMLS API
;;

(defun umls-concepts-for-word (word)
  (mapcar #'third (umls-term-recs word)))

(defun umls-concept-type (concept)
  (let ((rec (umls-type-rec concept)))
    (values (intern (second rec))
	    (fourth rec))))
       
(defun umls-concept-string (concept)
  (let ((rec (first (umls-detail-rec concept))))
    (values (nth 14 rec)
	    (nth 11 rec))))

(defun umls-concepts-for-type (type &aux concepts (last 0))
  "Ouch, linear time"
  (if (symbolp type) (setf type (symbol-name type)))
  (with-open-file (stream (get-umls-file 'concept-type))
    (loop 
       with size = (file-length stream)
       for line = (next-line stream)
       while line
       do (progn
	    (when (> (file-position stream) (+ last 5000000))
	      (format t "~2D%~%" (round (* 100 (/ (file-position stream) size))))
	      (setf last (file-position stream)))
	    (when (eq (compare-line line 1 type) :equal)
	      (push (first (line-fields line)) concepts)))))
  (nreverse concepts))

  
;;
;; UMLS Lookup Primitives
;;

(defun umls-definition-rec (concept-id)
  (assert (stringp concept-id))
  (first (umls-records-for-term (get-umls-file 'concept-def) 0 concept-id)))

(defun umls-detail-rec (concept-id)
  (umls-records-for-term (get-umls-file 'concept-detail) 0 concept-id))

(defun umls-type-rec (concept-id)
  (assert (stringp concept-id))
  (first (umls-records-for-term (get-umls-file 'concept-type) 0 concept-id)))

(defun umls-term-recs (term)
  (umls-records-for-term (get-umls-file 'norm-word-idx) 1 term))

;;
;; UMLS file utilities
;;

(defun umls-records-for-term (filename field-no value &aux lines)
  (with-open-file (stream filename)
    (let ((line (find-first-matching-line stream field-no value)))
      (loop 
	 while (eq (compare-line line field-no value) :equal)
	 do (progn (push (line-fields line) lines)
		   (setf line (next-line stream))))))
  (reverse lines))

(defun find-first-matching-line (stream field-no value)
  "Find the first matching line, presuming alphabetical ordering of field values"
  (let ((line (binary-find-matching-line stream field-no value 0 (file-length stream))))
    (cond ((< (file-position stream) 5000)
	   (file-position stream 0))
;;	  ((> (file-position stream) (- (file-length stream) 10000))
;;	   (file-position stream (- (file-length stream) 10000)))
	  (t (loop 
		while (and line (eq (compare-line line field-no value) :equal))
		do (progn (seek-back stream (* 10 (length line)))
			  (setf line (next-full-line stream))))))
    (seek-back stream (1+ (length line)))
    (linear-find-matching-line stream field-no value)))

(defun binary-find-matching-line (stream field-no value start end &optional old)
  "Given stream and current range, find a field matching value.
   Not guaranteed to be the first if there are duplicates"
  (let ((line (file-line-between stream start end)))
    (cond ((or (null line) (equal line old))
	   (seek-back stream (* 10 (length line)))
	   (linear-find-matching-line stream field-no value))
	  (t (ecase (compare-line line field-no value)
	       (:equal line)
	       (:gt (binary-find-matching-line stream field-no value start (file-position stream) line))
	       (:lt (binary-find-matching-line stream field-no value (file-position stream) end line)))))))

(defun seek-back (stream length)
  (file-position stream (max (- (file-position stream) length) 0)))

(defun linear-find-matching-line (stream field-no value)
  "Find the first matching line or nil"
  (loop 
     for line = (next-line stream)
     when (if (null line)
	      (return nil)
	      (case (compare-line line field-no value)
		 (:equal t)
		 (:gt (return nil))
		 (:lt nil)))
     do (return line)))

(defun compare-line (line field-no value)
  (compare-field (nth field-no (line-fields line)) value))

(defun line-fields (line)
  (split-sequence:split-sequence #\| line))

(defun compare-field (field value)
  (cond ((null field) nil)
	((= (length field) 0) nil)
	((string-equal field value) :equal)
	((umls-string> field value) :gt)
	(t :lt)))

(defun umls-string> (string1 string2)
  "Need to account for UMLS ordering over longest first"
  (let ((match (match-substring string1 string2)))
    (cond ((eq match :equal)
	   (< (length string1) (length string2)))
	  (t match))))

(defun match-substring (string1 string2)
  (assert (stringp string1))
  (assert (stringp string2))
  (loop 
     for char1 across string1
     for char2 across string2
     unless (char= char1 char2)
     do (return-from match-substring (char> char1 char2)))
  :equal)

(defun file-line-between (stream start end)
  (assert (> end start))
  (file-position stream (floor (+ start (/ (- end start) 2))))
  (next-full-line stream))

(defun next-full-line (stream)
  (read-line stream nil)
  (read-line stream nil))

(defun next-line (stream)
  (read-line stream nil))


;; Tests 

(defun umls-concepts-for-term (term)
  (umls-records-for-term (get-umls-file 'norm-word-idx) 1 term))

(defun test-find-line-between ()
  (with-open-file (stream (get-umls-file 'norm-word-idx))
    (file-line-between stream 0 (file-length stream))))

(defun test-find-line (word)
  (with-open-file (stream (get-umls-file 'norm-word-idx))
    (binary-find-matching-line stream 1 word 0 (file-length stream))))

(defun test-linear-find-line (word)
  (with-open-file (stream (get-umls-file 'norm-word-idx))
    (linear-find-matching-line stream 1 word)))

(defun test-find-first-line (word)
  (with-open-file (stream (get-umls-file 'norm-word-idx))
    (find-first-matching-line stream 1 word)))

