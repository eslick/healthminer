(in-package :smart)

(defun extract-tabbed-fields (line)
  (mapcar #'trim-non-alphanumeric
	  (utils::split (format nil "~A" #\Tab) line)))

(defun extract-tabbed-file (file &aux results)
  (with-open-file (stream file)
    (do-stream-lines (line stream)
      (push (extract-tabbed-fields line) results)))
  results)

;; ===============

(defun extract-cancer-table (file)
  (labels ((convert-count-field (field)
	     (if (or (null field)
		     (not (digit-char-p (char field 0))))
		 0
		 (read-from-string (string-remove-characters field '(#\,)))))
	   (convert-raw-fields (fields)
	     (cons (first fields)
		   (convert-count-field (third fields)))))
    (sort (mapcar #'convert-raw-fields 
		  (extract-tabbed-file file))
	  #'> :key #'cdr)))

(defun generate-json-cancer-table (file)
  (json:encode-json-alist-to-string 
   (extract-cancer-table file)))

(defun alist->csv (alist)
  (with-output-to-string (stream)
    (loop for cell in alist do
	 (format stream "~A, ~A~%" (car cell) (cdr cell)))
    (format stream "~%")))