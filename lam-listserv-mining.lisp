(in-package :smart)

;;;;
;;;; Bookkeeping
;;;;

(defun init-lam-listserv-db ()
  (init-langutils)
  (open-store '(:BDB "/users/eslick/work/db/lam-listserv/"))
  (ensure-stopword-hash)
  (get-message-map))

(defpclass dataset ()
  ((title :accessor dataset-title :initarg :title :initform "")
   (description :accessor dataset-description :initarg :description :initform "")
   (data :accessor dataset-data :initarg :data)
   (date :accessor dataset-date :initarg :date :initform (get-universal-time))))

(defun all-datasets ()
  (get-instances-by-class 'dataset))

(defmethod print-object ((obj dataset) stream)
  (format stream "#<DATASET '~A'>" (or (dataset-title obj) "Empty")))

(defpclass evaluation ()
  ((description :accessor evaluation-description :initarg :description :initform "")
   (dataset :accessor evaluation-dataset :initarg :dataset)
   (results :accessor evaluation-results :initarg :results :initform nil)
   (notes :accessor evaluation-notes :initarg :notes :initform "")
   (date :accessor evaluation-date :initarg :date :initform (get-universal-time))))

(defun all-evaluations ()
  (get-instances-by-class 'evaluation))

(defmethod print-object ((obj evaluation) stream)
  (format stream "#<EVALUATION '~A'>" (or (evaluation-description obj) "Empty")))


;; ================================================
;; Manipulating Sentence Windows
;; ================================================

(defmacro map-all-windows ((wvar wsize &optional (report 0)) &body body)
  (with-gensyms (windows msg-id rec)
    `(map-messages (,msg-id ,rec ,report)
       (loop 
	  with ,windows = (extract-sentence-windows (mmap-vdoc ,rec) ,wsize)
	  for ,wvar in ,windows
	  collect (progn ,@body)))))

(defun extract-sentence-windows (vdoc size)
  (remove-nulls
   (loop 
      with sentences = (document-sentence-phrases vdoc)
      with length = (length sentences)
      for offset from 0 upto length
      collect (if (< (- length offset) (min size 2))
		  (subseq sentences offset)
		  (subseq sentences offset (1- (+ offset (min size 2))))))))
;;   (maplist (lambda (sentences)
;;	      (when (>= (length sentences) size) 
;;		(subseq sentences 0 size)))
;;	    (document-sentence-phrases vdoc))))

(defun document-sentence-phrases (vdoc)
;;  (declare (optimize (speed 1) (safety 3)))
  (labels ((plen (start end)
	     (declare (type fixnum start end))
	     (min (1+ (- (or end (length (vector-document-words vdoc))) start))
		  (- (length (vector-document-words vdoc)) start)))
	   (phrase-gen (start ends)
	     (declare (type fixnum start)
		      (type cons ends))
	     (if (and ends (= start (first ends)))
		 (phrase-gen (car ends) (rest ends))
		 (cons (make-phrase-from-vdoc 
			vdoc start (plen start (when ends (car ends))))
		       (when ends 
			 (phrase-gen (1+ (car ends)) (rest ends)))))))
    (phrase-gen 0 (sort (append (term-positions "." (vector-document-words vdoc))
				(term-positions "?" (vector-document-words vdoc))
				(term-positions "!" (vector-document-words vdoc)))
			#'< ))))

;; ============================
;; Filter windows by terms
;; ============================

(defun all-term-filtered-windows (terms &optional (size 3))
  (let ((windows nil)
	(ids (sort (mapcar #'id-for-token terms) #'<)))
    (map-all-windows (window size)
      (when (some (lambda (word)
		    (member word ids :test #'equal))
		  (sort (apply #'append 
			       (mapcar #'phrase-words window))
			#'<))
	(when (= 0 (mod (length windows) 500)) (print (length windows)))
	(push window windows)))
    windows))

;; ============================
;; Extract lexical windows
;; ============================

(defun lexical-windows-for-term (pivot size &key max)
  (let ((windows nil)
	(docnum 0))
    (map-messages (id rec)
      (awhen (make-lexical-windows id rec pivot size)
	(push it windows)
	(when (and (not (null max)) (> (incf docnum) max))
	  (return-from lexical-windows-for-term (flatten1 windows)))))
    (flatten1 windows)))

(defun make-lexical-windows (id rec pivot wsize)
  (mapcar (lambda (position)
	    (make-lexical-window id rec position wsize))
	  (term-positions pivot (mmap-words rec))))

(defun make-lexical-window (id rec position size)
  (list id (position-window position size (mmap-words rec))))


;; ===================================
;;  Extract lexical pattern matches 
;; ===================================

(defun all-filtered-lexical-matches (pattern filter-terms &key max (phrase-size 20))
  (let ((smart-lex::*max-window-size* phrase-size))
    (declare (special smart-lex::*max-window-size*))
    (filter-matches (all-lexical-matches pattern max) filter-terms)))

(defun all-lexical-matches (pattern &optional max)
  (let ((matches nil)
	(docnum 0))
    (map-messages (id rec)
      (awhen (smart-lex:lexpat-matches pattern (mmap-vdoc rec))
	(push (mapcar (lambda (match)
			(cons id (filter-end-phrases pattern match)))
		      it)
	      matches)
	(when (and (not (null max)) (> (incf docnum) max))
	  (return-from all-lexical-matches (flatten1 matches)))))
    (flatten1 matches)))

(defun all-chunk-matches (pattern &optional max)
  "Return lexical pattern matches for all "
  (let ((matches nil)
	(docnum 0))
    (map-messages (id rec)
      (awhen (smart-lex:lexpat-matches pattern (mmap-vdoc rec))
	(push (mapcar (lambda (match)
			(cons id (match-chunks match (mmap-chunks rec))))
		      it)
	      matches)
	(when (and (not (null max)) (> (incf docnum) max))
	  (return-from all-chunk-matches (flatten1 matches)))))
    (flatten1 matches)))

(defun filter-matches (matches filter-terms)
  (let ((ftable (make-filter-table filter-terms)))
    (loop 
       for (id . match) in matches 
       for phrases = (flatten (remove-nulls (cdrs match)))
       when (= (length (filter-chunks phrases ftable)) (length phrases))
       collect (cons id match))))

(defun filter-end-phrases (pattern match)
  (when (symbolp (first pattern))
    (setf (cdr (first match))
	  (right-sentence (cdr (first match)))))
  (when (symbolp (last1 pattern))
    (setf (cdr (last1 match))
	  (left-sentence (cdr (last1 match)))))
  match)

(defparameter *sentence-boundaries* nil)

(defun sentence-boundaries ()
  (aif-ret *sentence-boundaries*
    (setf *sentence-boundaries*
	  (mapcar #'id-for-token '("." "?" "!")))))

(defun left-sentence (phrase)
  (with-slots (langutils::document langutils::start langutils::end) phrase
    (let ((text (document-text langutils::document))
	  (boundaries (sentence-boundaries)))
      (loop for pos from langutils::start upto langutils::end 
	 do (progn
	      (when (member (aref text pos) boundaries)
		(return-from left-sentence 
		  (make-phrase-from-vdoc 
		   langutils::document 
		   langutils::start 
		   (- pos langutils::start))))))
      phrase)))


(defun right-sentence (phrase)
  (with-slots (langutils::document langutils::start langutils::end) phrase
    (let ((text (document-text langutils::document))
	  (boundaries (sentence-boundaries)))
      (loop for pos from langutils::end downto langutils::start
	 do (progn
	      (when (member (aref text pos) boundaries)
		(return-from right-sentence 
		  (make-phrase-from-vdoc 
		   langutils::document 
		   (1+ pos)
		   (- langutils::end pos))))))
      phrase)))


(defun match-chunks (matches chunks)
  "Extract the document chunks for each matched variable's phrase"
  (loop for (label . phrase) in matches
       collect (cons label 
		     (phrases-in-window chunks
					(phrase-start phrase)
					(phrase-end phrase)))))

(defun phrases-in-window (phrases start end)
  "Given a list of phrases, extract all that fall on or between start/end"
  (select-if (lambda (phrase)
	       (and (>= (phrase-start phrase) start)
		    (<= (phrase-end phrase) end)))
;;		    (or (eq (phrase-type phrase) :nx)
;;			(eq (phrase-type phrase) :vx))))
	     phrases))

(defun longest-phrase (phrases)
  "Given a list of phrases, return the first longest phrase"
  (let ((max 0)
	(elt nil))
    (loop for phrase in phrases 
	 for len = (- (phrase-end phrase) (phrase-start phrase))
	 when (> len max)
	 do (setf max len
		  elt phrase))
    elt))

;;
;; Relation mining
;;


(defparameter *relation-lexical-patterns-take1* 
  '((cause "results" "in" effect)
    (cause "can" "lead" "to" effect)
    (effect "because" cause) ;; too generic
    (cause "causes" effect)
    (cause "can" "help" effect)
    (cause "which" "helps" effect)
    ("said" cause "could" "mean" effect)
    ("that" cause "could" "mean" effect)
    ("should" effect "if" cause)
    ("if" cause "then" effect)

    ("when" cause "," effect)
    (effect "caused" "by" cause)))

(defparameter *relation-lexical-patterns-take2* 
  '((cause "results" "in" effect)
    (cause "can" "lead" "to" effect)
    (cause "caused" effect)
    (cause "can" "help" effect)
    (cause "which" "helps" effect)
    ("said" cause "could" "mean" effect)
    ("that" cause "could" "mean" effect)
    ("should" effect "if" cause)
    ("if" cause "then" effect)
    ("when" cause "," effect)
    ("believe" effect "is" "due" "to" cause)
    (effect "caused" "by" cause)))

(defun stringify-match (match)
  (dbind (id &rest pairs) match
    (cons id
	  (loop for (name . phrase) in pairs collect
	       (cons name (phrase->string phrase))))))

(defun stringify-matches (matches)
  (mapcar #'stringify-match matches))

(defun lexical-relation-patterns (data)
  (car data))
      
(defun lexical-relation-data (data)
  (array->list (cdr data)))

(defun lexical-relation-list-to-unique-strings (matches)
  (remove-duplicates
   (stringify-matches matches)
   :test #'equalp :key #'cdr))

(defun all-lexical-relations (&optional (patterns *relation-lexical-patterns-take2*))
  (apply #'append 
	 (mapcar #'all-lexical-matches
		 patterns)))

(defun filtered-lexical-relations (filter-terms &optional (patterns *relation-lexical-patterns-take2*))
  (apply #'append
	 (mapcar (lambda (pattern)
		   (filter-matches (all-lexical-matches pattern) filter-terms))
		 patterns)))

(defun select-matches-by-term (term matches)
  (let ((results nil)
	(id (id-for-token term)))
    (loop for match in matches do
	 (loop for phrase in (cdrs (cdr match)) do
	      (when (member id (phrase-words phrase))
		(push match results))))
    results))

;; ===========================================================
;;  Domain Term Lists -> Chunks
;; ===========================================================

;; write for divisi ( Sentences, probe_terms )
;; load ( key terms )
;; extract filtered chunks -> 'term table'

(defun domainlist->term-table-dataset (filename min-count &key (description "") (wsize 3))
  "Take a list of terms and return a hash of string domain phrases
   that occur at least min-count times extracted from windows of size wsize"
  (let ((filter-terms (cleanup-neighborhoods
		       (json:decode-json-from-source filename)))
	(dataset (make-instance 'dataset :description 
				(format nil "~A (file: '~A', count-filter: ~A, wsize: ~A)"
					description filename min-count wsize))))
    (setf (dataset-data dataset)
	  (filter-term-table 
	   (all-filtered-corpus-chunk-terms filter-terms wsize)
	   min-count))
    dataset))
    
(defun all-filtered-corpus-chunk-terms (filter-terms wsize)
  (format nil "Extracting and filtering corpus chunks")
  (let ((ftable (make-filter-table filter-terms))
	(ttable (make-hash-table :test #'equal)))
    (map-all-windows (window wsize)
      (let ((chunks (filtered-corpus-chunk-terms window ftable)))
	(loop 
	   for chunk in chunks
	   for term = (chunk->concept chunk)
	   when term
	   do (incf-hash term ttable))))
    ttable))
  
(defun filtered-corpus-chunk-terms (window filter-table)
  (clean-chunks
   (filter-chunks
    (append (window-chunks window 'get-nx-chunks)
	    (window-chunks window 'get-event-chunks))
    filter-table)))

;; (defun make-term-table (chunks)
;;   (format nil "Making term table")
;;   (let ((hash (make-hash-table :test 'equal)))
;;     (dolist (chunk chunks)
;;       (let ((string (chunk->concept chunk)))
;; 	(mvbind (value has) (gethash string hash)
;; 	  (declare (ignore value))
;; 	  (if has 
;; 	      (incf (gethash string hash))
;; 	      (setf (gethash string hash) 1)))))
;;     hash))

(defun filter-term-table (chunk-table min-count)
  (format nil "Filtering term table")
  (let ((table (make-hash-table :test #'equal)))
    (loop for term being the hash-keys of chunk-table
	 using (hash-value count)
       when (and term (>= count min-count))
       do (setf (gethash term table) count))
    table))
	 

(defun get-divisi-term-pairs (document start position end)
  (let ((left-side (get-nx-chunks document (cons start position)))
	(right-side (get-nx-chunks document (cons position end))))
    (combinations (chunks->concepts (filter-simple-chunks left-side))
		  (chunks->concepts (filter-simple-chunks right-side)))))

(defun get-all-pivot-pairs (term filter-terms)
  (let ((filter-table (make-filter-table filter-terms)))
    (remove-nulls
     (mapcar (lambda (pair)
	       (when (= (length (filter-chunks pair filter-table)) 2)
		 pair))
	     (remove-nulls
	      (mapcan (lambda (message)
			(let ((doc (message-doc message)))
			  (loop 
			     for position in 
			     (term-positions term (vector-document-words doc))
			     nconc (get-pivot-pairs (message-doc message) position))))
		      (messages-for-word term)))))))

(defun get-pivot-pairs (document position)
  (let ((left-interval (cons (max 0 (- position 40)) 
			     position))
	(right-interval (cons (1+ position) 
			      (min (length (document-text document))
				   (+ position 40)))))
    (combinations (last2 (get-pivot-chunks document left-interval))
		  (first2 (get-pivot-chunks document right-interval)))))

(defun get-pivot-chunks (document interval)
  (sort (append (get-nx-chunks document interval)
		(get-event-chunks document interval))
	#'<
	:key #'phrase-start))

(defun first2 (list) (subseq list 0 (min 2 (length list))))
(defun last2 (list) (first2 (reverse list)))

  

(defun filter-simple-chunks (chunks)
  (remove-nulls 
   (loop 
      for chunk in chunks
      collect (loop 
		 for token in (phrase-lemmas chunk)
		 unless (filter-token-p token)
		 return chunk))))

(defun filter-token-p (token)
  (let ((string (token-for-id token)))
    (or (<= (length string) 2)
	(> (length string) 30))))

(defun chunks->concepts (chunks)
  (loop 
     for chunk in chunks 
     for concept = (chunk->concept chunk)
     when concept
     collect concept))

(defun chunk->concept (chunk)
  (concat-words 
   (cleaned-divisi-tokens (phrase-words chunk) :stopwords nil)))

(defun window-chunks (window &optional (extractor 'get-nx-chunks))
  (with-slots (langutils::document langutils::start) (first window)
    (with-slots (langutils::end) (last1 window)
      (funcall extractor langutils::document (cons langutils::start langutils::end)))))

(defun window-chunks (window &optional (extractor 'get-nx-chunks))
  (with-slots (langutils::document langutils::start) (first window)
    (with-slots (langutils::end) (last1 window)
      (select-if (f (chunk)
		   (and (< (phrase-start chunk) langutils::start)
			(> (phrase-end chunk) langutils::end)))
		 (funcall extractor langutils::document)))))

(defun make-filter-table (filter-list)
  (let ((filter-table (make-hash-table :test 'eq)))
    (mapc (lambda (term) (setf (gethash (id-for-token term) filter-table) t)) filter-list)
    filter-table))

;; ==============================================================
;;  Web PMI
;; ==============================================================

(defvar *hit-count-cache*  (make-hash-table :test #'equal))

(defparameter *lam-terms* '("lung" "lam" "pneumothorax" "chyle" "chest" "kidney" "trial" "rapamycin"))
  
(defun web-pmi (term1 term2 &key (engine :google))
  (/ (* (get-hit-count (format nil (and-query2-template engine) term1 term2)
		       :engine engine)
	60000000000)
     (1+ (* (get-hit-count term1 :engine engine)
	    (get-hit-count term2 :engine engine)))))

(defun get-hit-count (query &key (engine :google))
  (aif-ret (gethash query *hit-count-cache*)
    (setf (gethash query *hit-count-cache*)
	  (web-hit-counts query engine))))

(defun evaluate-termlist-pmi (dataset refs &key description subset (engine :google))
  (let (all averages)
    (labels ((return-evaluation ()
	       (let ((eval (make-instance 'evaluation 
					  :description (or description "PMI evaluation")
					  :dataset dataset
					  :notes refs)))
		 (setf (evaluation-results eval) (list all averages))
		 eval)))
      (restart-case
	  (dolist (term (subseq (dataset-data dataset) 0 subset))
	    (let ((sum 0) (count 0))
	      (dolist (ref refs)
		(let ((pmi (web-pmi term ref :engine engine)))
		  (when (not (= pmi 0)) (setf pmi (log pmi)))
		  (progn (incf count) (incf sum pmi))
		  (push (list term ref pmi) all)
		  (format t "~A (~A -> ~A)~%" pmi term ref)))
	      (push (list term (/ sum count)) averages)))
	(return-results ()
	  :report "Return results thus far"
	  (return-evaluation))))))

(defun average-of-terms (evaluation refs)
  (let ((averages (make-hash-table :test #'equal))
	(counts (length refs)))
    (loop for (term ref pmi) in (first (evaluation-results evaluation)) 
       when (member ref refs :test #'equal)
       do (incf-hash term averages pmi))
    (mapcar #'(lambda (key)
		(setf (gethash key averages)
		      (/ (gethash key averages) counts)))
	    (hash-keys averages))
    (hash-items averages)))


;; ===================================================================
;;  Corpus Analysis 
;; ===================================================================

;;; Message relations (hand labeled)

(defpclass message-relations ()
  ((message :accessor message :initarg :msg :index t)
   (type :accessor message-type :initarg :type :initform nil)
   (relations :accessor relations :initarg :relations)
   (description :accessor descripation :initarg :description)))

(defun get-message-relation (message)
  (get-instance-by-value 'message-relations 'message message))

(defun all-message-relations ()
  (get-instances-by-class 'message-relations))

(defmethod print-object ((obj message-relations) stream)
  (format stream "#<MSG-RELS [~A] for ~A>" (message-type obj)
	  (ele::oid obj)))

(defun get-message-type ()
  (format t "~%What type of message? ")
  (let ((type (read)))
    (if (member type '(answer info story sympathy thankyou junk 
		       discussion question))
	type
	(get-message-type))))

(defun hand-label-relations (message)
  (assert (eql (type-of message) 'message))
  (print-message-body message)
  (let ((relations nil)
	(type (get-message-type)))
    (format t "~%Record relation (quit/empty/undo/record): ")
    (loop for relation = (read-line)
	 while (> (length relation) 0)
	 do (progn
	      (if (= (length relation) 1)
		  (case (elt relation 0)
		    (#\q (return-from hand-label-relations nil))
		    (#\e (setf relations nil))
		    (#\u (setf relations (rest relations))))
		  (push (read-from-string relation) relations))
	      (format t "Record relation: ")))
    (make-instance 'message-relations :msg message :type type
		   :relations relations)))

(defun hand-label-messages (messages)
  (labels ((continue () (hand-label-messages (rest messages)))
	   (repeat () (hand-label-messages messages))
	   (msg () (first messages)))
    (acond ((null messages) nil)
	   ((get-message-relation (msg))
	    (unless (message-type it)
	      (print-message-body (msg))
	      (setf (message-type it)
		    (get-message-type)))
	    (continue))
	   ((null (hand-label-relations (msg)))
	    (repeat))
	   (t (continue)))))

(defun message-relation-distribution ()
  (let ((hist (histogram 
	       (mapcar #'message-type 
		       (all-message-relations)))))
    (format t "~A% messages of ~A have useful info" 
	    (* 100 (coerce (/ (+ (cdr (assoc 'answer hist))
				 (cdr (assoc 'info hist)))
			      (apply #'+ (cdrs hist)))
			   'float))
	    (apply #'+ (cdrs hist)))
    hist))

;;(defun recall-of-labeled-messages (evaluation)
  

(defparameter *weighted-messages* (make-hash-table))

(defun weigh-messages (terms)
  (setf *weighted-messages* (make-hash-table))
  (map-messages (msg-id rec)
    (let* ((words (vector-document-words (mmap-vdoc rec)))
	   (size (length words)))
      (when (> size 20)
	(loop for word in (sorted-word-counts (tokens-for-ids words) :test #'equal)
	   when (member word terms :test #'equal)
	   do (if #2=(gethash msg-id *weighted-messages*)
		  (incf #2# (update-entropy-weight #2#))
		  (setf #2# (update-entropy-weight 0)))))))
  t)

;;(defun update-entropy-weight (current)
;;  (+ current (1+ (log (* 
  

;;; Word distributions

(defparameter *word-counts* nil)

(defun make-message-word-counts ()
  (let ((hash (make-hash-table :size 100000 :test 'equalp))
	(count 0))
    (setf *word-counts* hash)
    (map-class (lambda (inst)
		 (count-words (body inst) hash)
		 (when (= 0 (mod (incf count) 100))
		   (print count)))
	       'message)
    hash))

(defun normalize-string (string)
  (string-downcase 
   (trim-non-alphanumeric string)))

(defun incf-hash (key hash &optional (value 1))
  (unless (gethash key hash)
    (setf (gethash key hash) 0))
  (incf (gethash key hash) value))

(defun count-words (text hash)
  (loop for word in (extract-words text)
       for dword = (get-lemma (string-downcase word))
       unless (stopword-p dword)
       do (incf-hash dword hash)))

(defun word-count (word)
  (gethash (get-lemma (string-downcase word)) *word-counts*))


;;     (labels ((find-string (start &aux pos) 
;; 	       (while (and (< start text-size)
;; 			   (not (alphanumericp (char text start))))
;; 		 (incf start))
;; 	       (setf pos start)
;; 	       (while (and (< pos text-size)
;; 			   (alphanumericp (char text pos)))
;; 		 (incf pos))
;; 	       (when (> pos text-size)
;; 		 (return-from count-words))
;; 	       (let ((str (ref-string start (- pos start))))
;; 		 (unless (stopword-p (string-downcase str))
;; 		   (incf-hash str hash)))
;; 	       (find-string (1+ pos)))
;; 	     (ref-string (start length)
;; 	       (make-array length :displaced-to text :displaced-index-offset start 
;; 			   :element-type 'character)))
;;       (find-string 0))))

;;  (mapc (lambda (wordstring)
;;	  (unless (stopword-p wordstring)
;;	    (incf-hash wordstring hash)))
;;	(split-sequence:split-sequence #\space (normalize-string text)))
;;  hash)

(defun sorted-word-counts (hash &aux pairs)
  (maphash (lambda (word count)
	     (when (> count 1)
	       (push (cons (string-downcase word) count) pairs)))
	   hash)
  (sort pairs #'> :key #'cdr))

(defun corpus-words (&aux (count 0))
  (map-messages (id rec)
    (incf count (length (mmap-lemmas rec))))
  count)

;;
;; PMI within corpus
;;

(defparameter *pmi-window-size* 20)

(defun term-pmi (word1 word2)
  (coerce (/ (window-counts (list word1) word2 *pmi-window-size*)
	     (word-count word2))
	  'float))

(defun term-pivot-pmi (pivot word1 word2)
  (coerce (/ (window-counts (list word1 word2) pivot *pmi-window-size*)
	     (* (word-count pivot) (word-count word1) (word-count word2)))
	  'float))

;;
;; User distributions
;;

(defparameter *sorted-senders* nil)

(defun top-posters (n)
  (top-n (aif-ret *sorted-senders* 
	   (generate-sorted-senders))
	 n))

(defun generate-sorted-senders ()
  (let ((hash (make-hash-table :test #'equal)))
    (map-class 
     (lambda (message)
       (with-slots (sender) message
	 (if (hash-contains hash sender)
	     (incf (gethash sender hash))
	     (setf (gethash sender hash) 1))))
     'message)
    (setf *sorted-senders* 
	  (sort (hash-items hash) #'> :key #'cdr))))

(defun top-n (list n)
  (subseq list 0 n))


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
