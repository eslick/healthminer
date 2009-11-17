(in-package :smart)

(defun most-common-nv-ngrams (terms &key (size 7) max (min-counts 2) (n 2))
  (let ((*lexical-window-elements* 'message-rec-head-terms))
    (declare (special *lexical-window-elements*))
    (most-common-lexical-ngrams terms :size size :max max :min-counts min-counts :n n
				:intersection t)))

(defun most-common-lexical-ngrams (terms &key (size 7) max (min-counts 2) (n 2) intersection)
  (select-if 
   (f (rec) (> (cdr rec) min-counts))
   (sorted-sublist-counts
    (let ((term-ngrams (loop for term in terms 
			  collect (all-lexical-ngrams term :size size :max max :stopwords t :n n))))
      (if intersection
	  (reduce (f (a b) (intersection a b :test #'equalp)) term-ngrams)
	  (flatten1 term-ngrams))))))

(defun all-lexical-ngrams (term &key (size 7) max sort stopwords (n 2))
  (labels ((rec (left right patterns)
	     (if (not patterns)
		 (values left right)
		 (rec (nconc (left-ngrams term (first patterns) :n n) left)
		      (nconc (right-ngrams term (first patterns) :n n) right)
		      (rest patterns)))))
    (mvbind (l r) 
	(rec nil nil (patterns-for-term term :size size :max max :strings t))
      (when stopwords 
	(setf l (remove-stopword-ngrams l))
	(setf r (remove-stopword-ngrams r)))
      (if sort 
	  (values (sorted-sublist-counts l) (sorted-sublist-counts r))
	  (values l r)))))
     
(defun remove-stopword-ngrams (ngrams)
  (loop 
     for ngram in ngrams 
     when (not (ngram-stopword? ngram))
     collect ngram))

(defun ngram-stopword? (ngram)
  (and (= (length (car ngram)) 1)
       (stopword? (id-for-token (first (car ngram))))))

(defun sorted-sublist-counts (list)
  (sort (histogram list) #'> :key #'cdr))

(defun patterns-for-term (term &key (size 7) (max nil) (strings nil))
  (let ((windows (lexical-windows-for-term term size :max max)))
    (if strings
	(mapcan
	 (f (rec)
	   (dbind (mid &rest patterns) rec
	     (mapcar #'tokens-for-ids patterns)))
	 windows)
	windows)))

(defun message-rec-head-terms (rec)
  (let ((doc (mmap-vdoc rec)))
    (loop 
       for tag across (document-tags doc)
       for id across (document-text doc)
       when (or (verb-pos-p tag) (noun-pos-p tag))
       collect id)))

;;
;; ngrams
;;

(defun left-ngrams (terms sequence &key (n 2))
  (loop 
     with ne = (mklist terms)
     for size from 1 upto n
     for left = (left-window ne sequence)
     nconc (add-ngram-offsets (reverse (ngrams size left)))))

(defun left-window (terms sequence)
  (subseq sequence 0 (position (first terms) sequence :test #'equal)))

(defun right-ngrams (terms sequence &key (n 2))
  (loop 
     with ne = (mklist terms)
     for size from 1 upto n
     for right = (right-window ne sequence)
     nconc (add-ngram-offsets (ngrams size right))))

(defun right-window (terms sequence)
  (subseq sequence (1+ (position (last1 terms) sequence :test #'equal))
	  (length sequence)))

(defun add-ngram-offsets (list)
  (loop 
     for ngram in list 
     for size from 0
     collect (cons ngram size)))

(defun ngrams (size sequence &optional (offset 0))
  (when (and sequence (>= (length sequence) (+ size offset)))
    (cons (subseq sequence offset (+ offset size))
	  (ngrams size sequence (1+ offset)))))


