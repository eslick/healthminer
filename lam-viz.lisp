(in-package :smart)

;; Quick and dirty visualizations with ManyEyes
;; Ideas: combined score of frequency in corpus to dict freq.

(defun generate-word-cloud (filename terms &optional truncate)
  (labels ((get-subset (counts)
	     (if truncate
		 (subseq (sort counts #'> :key #'cdr) 0 truncate)
		 counts))
	   (normalize-subset (counts)
	     (let ((min (reduce #'min counts :key #'cdr)))
	       (loop for count in counts
		    collect (cons (car count) (ceiling (/ (cdr count) min)))))))
    (with-output-file (stream filename)
      (let ((counts (loop for term in (remove-duplicates 
				       (remove-if #'string-stopword? terms)
				       :test #'equalp)
			 for count = (word-count term)
			 when count
			 collect (cons term count))))
	(loop for pair in (normalize-subset (get-subset counts)) do
	     (loop for i from 0 upto (cdr pair) do
		  (format stream "~A " (car pair)))
	     (format stream "~%"))))))

;; Generate a sample of raw data

(defun generate-message-sample (filename count)
  (with-output-file (stream filename)
    (loop for mrec in (random-subset (hash-values *message-map*) count) do
	 (format stream "~A~%"
		 (concat-words 
		  (remove-if (f_ (member _ '("unmask" "log" "n't" "cgi-bin" "href" "wa.exe"
					     "font-family" "ec" "http" "mailto" "a3"
					     "html" "dtd" "w3c" "transitional" "en" "div"
					     "target" "font-size")
					 :test #'equal))
			     (cleaned-divisi-tokens (vector-document-words (mmap-vdoc mrec))
						    :stopwords nil :qualifiers nil)))))))


;; Generate a sample of raw sentences containing key terms

(defun generate-term-sentence-sample (filename terms size)
  (with-output-file (stream filename)
    (let ((windows (all-term-filtered-windows terms size)))
      (loop for window in windows do
	   (format stream "~A~%" (concat-words (tokens-for-ids (phrase-words (first window)))))))))


;; Generate simple distributions

