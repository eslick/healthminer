(in-package :smart)

;;
;; Ontology
;;

(defparameter *key-terms* 
  '(difficult painful wheezing coughing transplant operation better
    lung breath breathing chest chyle pneumothorax pneumo
    pft fev fec humidity fat diet pneumonia humidity 
    altitude flying))

(defun dump-probe-terms (filename)
  (with-output-file (stream filename)
    (json:encode-json 
     (mapcar #'string-downcase (mapcar #'ensure-string *key-terms*))
     stream)))

(defun cleanup-neighborhoods (neighborhoods)
  (loop for neighborhood in neighborhoods nconc
       (dbind (term far near) neighborhood
	 (declare (ignore far term))
	 (filter-bad-terms 
	  (mapcar #'caar near)))))

(defun filter-bad-terms (terms)
  (remove-if #'bad-term-p terms))

(defun bad-term-p (term)
  (or (some (lambda (char)
	      (let ((code (char-code char)))
		(or (> code 128)
		    (= code (char-code #\-))
		    (= code (char-code #\.))
		    (and (>= code (char-code #\0))
			 (<= code (char-code #\9))))))
		    
	    term)
      (> (length term) 20)
      (ppcre:scan "[div|p|li|span]\\." term)))
      
;; =====================================================================
;;  Dump docs for LSA
;; =====================================================================

(defparameter *window-size* 1)

(defun dump-docs-for-lsa (dir &optional max &aux (docnum 0))
  "This creates a set of documents in a directory for LSA analysis.
   We also dump a file that correlates documents to their sources"
  (with-output-file (map (merge-pathnames dir "map"))
    (write "(" :stream map)
    (unwind-protect
	 (map-messages (id rec)
	   (loop for window in (extract-sentence-windows (mmap-vdoc rec) *window-size*) do
		(when (and (not (null max)) (> docnum max))
		  (return-from dump-docs-for-lsa t))
		(write-window-document window (doc-path dir (incf docnum)))
		(write (list docnum id (window-offset window)) :stream map)
		(write (format nil "~%") :stream map)))
      (write (format nil ")~%") :stream map))))

(defun dump-sentences-to-divisi (filename &key filter-list max)
  (let* ((count 0)
	 (filter-table (make-filter-table filter-list)))
    (with-output-file (stream filename)
      (json:encode-json 
       (remove-nulls 
	(flatten1
	 (map-all-windows (window *window-size*)
	   (let ((terms (sentence-window-terms window filter-table)))
	     (cond ((and max (> count max)) (return))
		   (terms
		    (list (incf count) terms)))))))
       stream))))

(defun sentence-window-terms (window &optional filter-table)
  (when window
    (append (cleaned-divisi-tokens (mapcan #'phrase-words window))
	    (when filter-table
	      (sentence-window-pairs window filter-table)))))

(defun sentence-window-pairs (window filter-table)
  "Given terms to pair with, extract phrase pairs containing a term"
  (phrase-pair-strings
   (filter-overlaps
    (unique-pairs
     (filter-chunks 
      (append (window-chunks window 'get-nx-chunks)
	      (window-chunks window 'get-event-chunks))
      filter-table)))))

;;
;; Using DIVISI results
;;

(defun divisi-windows-for-term (pivot size &key max)
  (let ((windows nil)
	(docnum 0))
    (map-messages (id rec)
      (awhen (make-divisi-windows id rec pivot size)
	(push it windows)
	(when (and (not (null max)) (> (incf docnum) max))
	  (return-from divisi-windows-for-term (flatten1 windows)))))
    (flatten1 windows)))

(defun make-divisi-windows (id rec pivot wsize)
  (let ((lpivot (token-for-id (get-lemma-for-id (id-for-token pivot)))))
    (mapcar (lambda (position)
	      (make-divisi-window id rec lpivot position wsize))
	    (term-positions pivot (mmap-lemmas rec)))))

(defun make-divisi-window (id rec pivot position size)
  (multiple-value-bind (terms start end)
      (position-window position size (mmap-lemmas rec))
    (list id pivot
	  (cleaned-divisi-tokens terms :stopwords nil)
	  (get-divisi-term-pairs (mmap-vdoc rec) start position end))))

(defun cleaned-divisi-tokens (terms &key (stopwords t) (qualifiers t))
  (let ((cleaned 
	 (mapcar #'trim-non-alphanumeric
		 (mapcar #'string-downcase 
			 (remove-if (lambda (token)
				      (let ((len (length token)))
					(or (> len 30)
					    (not (alpha-char-p (char token 0)))
					    (bad-chars-p token))))
				    (tokens-for-ids 
				     (remove-if (lambda (term)
						  (and stopwords
						       (or (punctuation? term)
							   (stopword? term))))
						terms)))))))
    (if qualifiers (remove-leading-qualifiers cleaned) cleaned)))

(defun bad-chars-p (string)
  (map-across (lambda (char)
		(when (> (char-code char) 128) (return-from bad-chars-p t)))
	      string)
  nil)

(defun filter-window-pairs (window-recs phrases)
  (flatten1
   (loop for rec in window-recs collect
       (dbind (id pivot terms pairs) rec
	 (declare (ignorable id pivot terms))
	 (select-if (lambda (pair)
		      (and (member (first pair) phrases :test #'equal)
			  (member (second pair) phrases :test #'equal)))
		    pairs)))))

(defparameter *qualifiers* '("the" "a" "an" "our" "your" "my" "their" "and" "with"
			     "his" "her"))

(defun remove-leading-qualifiers (terms)
  (cond ((null terms) nil)
	((member (first terms) *qualifiers* :test #'equal)
	 (remove-leading-qualifiers (cdr terms)))
	(t terms)))

(defparameter *punctuation* nil)

(defun get-punctuation ()
  (unless *punctuation*
    (setf *punctuation*
	  (mapcar #'id-for-token '("," ":" ";" "." "'s" "{" "}" "(" ")"
				   "[" "]" "<a" "href=" "wa.exe" "cgi-bin"
				   "/" "!" "'"))))
  *punctuation*)

(defun punctuation? (term)
  (member term *punctuation*))

(defun dump-windows-to-divisi (windows filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede
			  :external-format :utf-8 :if-does-not-exist :create)
    (json:encode-json 
     (mapcar #'(lambda (window)
		 (destructuring-bind (id pivot terms pairs) window
		   (declare (ignore id))
		   (list pivot (append terms pairs))))
	     windows)
     stream)))

(defun dump-causal-windows-to-divisi (filename &optional (max 1000) (verbs *causal-verbs*))
  (dump-windows-to-divisi 
   (loop for pivot in verbs nconc
	(divisi-windows-for-term pivot 40 :max max) stream)
   filename))

(defun dump-messages-to-divisi (filename)
  "Generate a structure to create a tensor of words to message ids"
  (with-output-file (stream filename)
    (json:encode-json
     (remove-nulls
      (map-messages (msg-id rec)
	(awhen (cleaned-divisi-tokens 
		(vector-document-words (mmap-vdoc rec)))
	  (list msg-id it))))
     stream)))

;;(defun window-counts (words pivot size)
;;  "The 'hits' for wordlist within size of word2"
;;  (length (conjunctive-windows (cons pivot words) size)

(defun conjunctive-windows (words size)
  (loop for window in (term-lexical-windows (car words) (* 2 size))
       when (every (lambda (word)
		     (member (get-lemma word) window))
		   (cdr words))
       collect window))


(defun clean-chunks (chunks)
  (filter-if 
   (lambda (chunk)
     (or (> (length (phrase-words chunk)) 10)
	 (some #'punctuation? (phrase-words chunk))))
   chunks))

(defun phrase-pair-strings (pairs)
  (loop 
     for (first second) in pairs 
     collect (list (chunk->concept first)
		   (chunk->concept second))))

(defun filter-overlaps (pairs)
  (remove-if (lambda (pair)
	       (dbind (first second) pair
		 (or (null first)
		     (null second)
		     (eq first second)
		     (phrase-contains-point (phrase-start first) second)
		     (phrase-contains-point (phrase-start second) first))))
	     pairs))

(defun filter-chunks (chunks table)
  (declare (type list chunks)
	   (type hash-table table)
	   (optimize (speed 3) (space 1) (safety 1) (debug 2)))
  (if (and chunks table (not (= (hash-table-count table) 0)))
      (select-if (lambda (chunk)
		   (when (subtypep (type-of chunk) 'phrase)
		     (let ((words (phrase-words chunk)))
		       (declare (type list words))
		       (and (not (every #'stopword? words))
			    (some (lambda (word)
				    (declare (type fixnum word))
				    (gethash word table))
				  words)))))
		 chunks)
      chunks))

;;;
;;; OLD CODE
;;;

(defun write-window-document (window filename)
  (with-output-file (doc filename)
    (write-string
     (merge-or-regex-strings
      (list (window-token-string window)
	    (window-term-pairs-string window))
      #\Space)
     doc)))

(defun window-term-pairs-string (window)
  (merge-or-regex-strings 
   (mapcar #'chunk-pair-string 
	   (unique-pairs (window-nx-chunks window)))
   #\Space))

(defun chunk-pair-string (chunks)
  (ppcre:regex-replace-all 
   " " 
   (concatenate 'string
     (print-phrase-lemmas (first chunks))
     "<>"
     (print-phrase-lemmas (second chunks)))
   ":"))


(defun doc-path (dir num)
  (merge-pathnames dir (format nil "d~A" num)))

(defparameter *sentence-separator* ".")


;; overlap
;; add pair tokens

(defun window-token-string (window)
  (merge-or-regex-strings 
   (mapcar (compose (curry2 'merge-or-regex-strings #\Space)
		    'tokens-for-ids
		    'phrase-lemmas)
	   window)
   #\Space))

(defun window-offset (window)
  (phrase-start (first window)))
