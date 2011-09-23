(in-package :smart)

(defmethod web-hit-counts (query engine)
  (restart-case 
      (extract-page-count (web-search query engine) engine)
    (retry-request (&optional new-engine)
      :report "Restart the search request?"
      (web-hit-counts query (or new-engine engine)))))

(defun extract-page-count (str engine)
  (multiple-value-bind (match count)
      (cl-ppcre:scan-to-strings (count-scanner engine) str :sharedp t)
    (declare (ignore match))
    (if count
	(values (read-from-string (remove #\, (aref count 0)))) 
	(error "Request to ~A failed" engine))))

(defun count-scanner (engine)
  (ecase engine
    (:google (load-time-value (ppcre:create-scanner "of about <b>([^<]*)</b>")))
    (:altavista (load-time-value (ppcre:create-scanner "found ([^ ]*) results")))))

(defun and-query2-template (engine)
  (ecase engine
    (:google "~A ~A")
    (:altavista "~A ~A")))


;;;;
;;;; Engines
;;;;

(defmethod web-search (query (engine (eql :google)))
  (drakma:http-request "http://www.google.com/search"
		       :parameters `(("q" . ,(format nil "allintext: ~A" query))
				     ("ie" . "utf-8")
				     ("oe" . "utf-8")
				     ("client" . "mozilla"))
		       :user-agent "Mozilla/5.0 (Macintosh; U; PPC Mac OS X Mach-O; en-US; rv:1.8.0.1) Gecko/20060111 Firefox/1.5.0.1"))

(defmethod web-search (query (engine (eql :altavista)))
  (drakma:http-request "http://altavista.com/web/results"
		       :parameters `(("q" . ,query))
		       :accept "text/html,text/plain"
		       :user-agent "Mozilla/5.0 (Macintosh; U; PPC Mac OS X Mach-O; en-US; rv:1.8.0.1) Gecko/20060111 Firefox/1.5.0.1"))

;;  (defun yahoo-search (query &key (pages 1) (type :web) &aux (retries 1))
;;    (loop for count from 1 upto (ceiling pages 50)
;;       nconcing
;;  	(handler-case
;; 	    (mapcar #'cl-yahoo:url (cl-yahoo:ysearch type
;;  						  :query query
;;  						  :adult_ok nil 
;;  						  :similar_ok nil
;;  						  :start (+ (* (1- count) 50) 1)
;;  						  :results 50))
;;  	 (error () 
;;  		(if (> (incf retries) 60)
;;  		    (error "Unable to access Yahoo query server for 2 minutes.")
;;  		  (progn
;;  		    (write-log miner "Failed to access yahoo, waiting to retry...")
;;  		    (sleep 5)
;;  		    (setf count (- count 50))
;;  		    nil))))))


