(in-package :smart)

;; drakma, 

(defparameter site-base-url "http://listserv.uc.edu")

(defparameter monthly-archive-links 
  '(("/cgi-bin/wa.exe?A1=ind0803&L=lam-patients" March 2008)
    ("/cgi-bin/wa.exe?A1=ind0802&L=lam-patients" February 2008)
    ("/cgi-bin/wa.exe?A1=ind0801&L=lam-patients" January 2008)
    ("/cgi-bin/wa.exe?A1=ind0712&L=lam-patients" December 2007)
    ("/cgi-bin/wa.exe?A1=ind0711&L=lam-patients" November 2007)
    ("/cgi-bin/wa.exe?A1=ind0710&L=lam-patients" October 2007)
    ("/cgi-bin/wa.exe?A1=ind0709&L=lam-patients" September 2007)
    ("/cgi-bin/wa.exe?A1=ind0708&L=lam-patients" August 2007)
    ("/cgi-bin/wa.exe?A1=ind0707&L=lam-patients" July 2007)
    ("/cgi-bin/wa.exe?A1=ind0706&L=lam-patients" June 2007)
    ("/cgi-bin/wa.exe?A1=ind0705&L=lam-patients" May 2007)
    ("/cgi-bin/wa.exe?A1=ind0704&L=lam-patients" April 2007)
    ("/cgi-bin/wa.exe?A1=ind0703&L=lam-patients" March 2007)
    ("/cgi-bin/wa.exe?A1=ind0702&L=lam-patients" February 2007)
    ("/cgi-bin/wa.exe?A1=ind0701&L=lam-patients" January 2007)
    ("/cgi-bin/wa.exe?A1=ind0612&L=lam-patients" December 2006)
    ("/cgi-bin/wa.exe?A1=ind0611&L=lam-patients" November 2006)
    ("/cgi-bin/wa.exe?A1=ind0610&L=lam-patients" October 2006)
    ("/cgi-bin/wa.exe?A1=ind0609&L=lam-patients" September 2006)
    ("/cgi-bin/wa.exe?A1=ind0608&L=lam-patients" August 2006)
    ("/cgi-bin/wa.exe?A1=ind0606&L=lam-patients" June 2006)
    ("/cgi-bin/wa.exe?A1=ind0605&L=lam-patients" May 2006)
    ("/cgi-bin/wa.exe?A1=ind0604&L=lam-patients" April 2006)
    ("/cgi-bin/wa.exe?A1=ind0603&L=lam-patients" March 2006)
    ("/cgi-bin/wa.exe?A1=ind0602&L=lam-patients" February 2006)
    ("/cgi-bin/wa.exe?A1=ind0601&L=lam-patients" January 2006)))

;; fetching

(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))

(defun get-main-page ()
  (fetch-relative-url "/archives/lam-patients.html"))

(defun fetch-relative-url (rel)
  (fetch-html-page (strcat site-base-url rel)))

(defun fetch-html-page (url)
  (net.aserve.client:do-http-request url :timeout 20 :user-agent "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)"))

(defun get-archive-pages ()
  (loop for (url month year) in monthly-archive-links 
       collecting
       (aif-ret (lookup-archive-page (cons month year))
		(let ((page (fetch-relative-url url)))
		  (format t "Fetched: ~A, ~A~%" month year)
		  (add-to-root (cons month year) page :sc *store-controller*)
		  page))))

(defun lookup-archive-page (ref)
  (get-from-root ref :sc *store-controller* ))

;; extract message urls

(defun all-register-groups (regex text &key sharedp &aux matches)
  (ppcre:do-register-groups (context) (regex text (nreverse matches) :sharedp sharedp)
    (push context matches)))

(defun get-all-urls (page)
  (all-register-groups "<a href=\"(.*)\">" page :sharedp t))

(defun select-message-urls (urls)
  (select-if (lambda (url) 
	       (search "&P=" url)) 
	     urls))

(defun get-message-urls (archive-page)
  (select-message-urls
   (get-all-urls archive-page)))


;; strip html markup

(defvar *html-filter-delete-scanner-text*
  '(""
    "\&\\S+"))

(defparameter *html-quick-filters*
  (list (make-regex-verify-filter "<html>|<HTML>|htm|shtml|SHTML" "Verify this is an html document")
	(make-regex-replace-filter (merge-or-regex-strings *html-filter-delete-scanner-text*) "" "Remove dos line feeds")
	(make-regex-replace-filter "\\s+" "  " "Truncate Whitespace")))

(defparameter *html-filters* 
  (list ;; (make-regex-verify-filter "<html>|<HTML>|htm|shtml|SHTML" "Verify this is an html document")
	(make-regex-replace-filter (merge-or-regex-strings *html-filter-delete-scanner-text*) "" "Remove dos line feeds")
	(make-regex-replace-filter "\\s+" "  " "Truncate Whitespace")
	(make-regex-replace-filter "<.*?>" " " "Remove Tags")
	(make-regex-replace-filter "\\s+" "  " "Truncate Whitespace")))

(defun strip-raw-html-fast (html)
;;  (cllib::strip-html-markup 
  (error "Need to define strip-html-markup better")
  (filter-text html *html-quick-filters*))

(defun strip-raw-html-regex (html)
  (filter-text html *html-filters*))

;; parse time string

(defparameter *message-recognizers*
  (list (utils::make-fmt-recognizer "%a, %d %b %Y %H:%M:%S %Z")))

(defun parse-message-time (string)
  (parse-time-string string  *message-recognizers*))

;; extract message text

(defun find-last-span (msg)
  (ppcre:scan "<span ID=\"MSGHDR-Content-Type-PRE\">[^<]+</span>" msg))

(defun find-end-position (text &optional (start 0))
  (let ((last (1- (length text))))
    (min (or (ppcre:scan "wrote:" text :start start) last)
	 (or (ppcre:scan "From:" text :start start) last)
	 (or (ppcre:scan "Back to:" text :start start) last)
	 (or (ppcre:scan "-----" text :start start) last))))

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter header-spans
    '((date "MSGHDR-Date-PRE")
      (sender "MSGHDR-From-PRE")
      (subject "MSGHDR-Subject-PRE")))

  (defun create-header-regex (header-tag)
    (format nil "<span ID=\"~A\">([^&<]+)" header-tag))

  (defun generate-header-extractor (name pagevar)
    `(,name
      (ppcre:register-groups-bind (data) 
	  (,(create-header-regex (second (assoc name header-spans))) ,pagevar)
	(trim-non-alphanumeric data))))

  (defun generate-header-extractors (names pagevar)
    (collect (lambda (name)
	      (when (member name header-spans :key #'first)
		(generate-header-extractor name pagevar)))
	     names))

  (defun generate-extract-body (pagevar)
    (with-gensyms (page start end)
      `(let* ((,page ,pagevar)
	      (,start (mvretn 2 (find-last-span ,page)))
	      (,end (find-end-position ,page ,start)))
	 (collapse-whitespace
	  (strip-raw-html-regex 
	   (subseq ,page ,start ,end))))))
  )

(defmacro with-message-parts (names page &body body)
  (with-gensyms (pageval)
    `(let ((,pageval ,page))
       (let (,@(generate-header-extractors names pageval))
	 (let ((body ,(generate-extract-body pageval)))
	   ,@body)))))

;; record all messages

(defpclass message ()
  ((url :accessor relative-url :initarg :url :index t)
   (sender :accessor sender :initarg :sender :index t)
   (subject :accessor subject :initarg :subject)
   (date :accessor date :initarg :date :index t)
   (udate :accessor udate :derived-fn (lambda (inst)
					(values (parse-message-time (date inst)) t))
	  :slot-deps (date))
   (body :accessor body :initarg :body)))
   

(defparameter *print-message-body* nil)

(defmethod print-object ((msg message) stream)
  (with-slots (date sender subject body) msg
    (format stream "#<LAM-MSG \"~A\" | \"~A\">"
	    sender date)))

(defmethod print-message-header (msg &optional (stream t))
  (with-slots (date sender subject body) msg
    (format stream "Date: ~A~%From: ~A~%Subject: ~A~%~%"
	    date sender subject)))

(defmethod print-message-body (msg &optional (stream t))
  (print (body msg) stream))

(defparameter *failed-url-log* nil)

(defun ensure-failed-url-log ()
  (when (null *failed-url-log*)
    (let ((btree (get-from-root '*failed-url-log*)))
      (when (null btree)
	(setf btree (make-btree))
	(add-to-root '*failed-url-log* btree))
      (setf *failed-url-log* btree))))

(defun log-failed-url (url etype)
  (ensure-failed-url-log)
  (setf (get-value url *failed-url-log*) etype))

(defun create-message (url message-page)
  (with-message-parts (date sender subject body) message-page
    (when (< (length body) 10) 
      (error "Truncated message body for url: ~A" url))
    (with-transaction (:txn-nowait t :txn-nosync t)
      (make-instance 'message
		     :url url
		     :date date
		     :sender sender
		     :subject subject
		     :body body))))

;; archive-all-messages

(defun rebuild-index (class indexed-slot)
  (let ((index (find-inverted-index class indexed-slot)))
    (map-btree (lambda (k v)
		 (remove-current-kv))
	       index)
    (map-class (lambda (inst)
		 (setf (get-value (slot-value inst indexed-slot) index) (elephant::oid inst)))
	       class)))

(defun get-message (url)
  (get-instance-by-value 'message 'url url))

(defun archive-message (url)
  (handler-case 
      (unless (get-message url)
	(print-message-header (create-message url (fetch-relative-url url))))
    (error (e)
      (log-failed-url url (type-of e))))
  (finish-output))

(defun archive-all-messages ()
  (dolist (page (get-archive-pages))
    #+allegro (excl:gc t)
    (dolist (url (get-message-urls page))
      (archive-message url))))
     
;; Stopwords into db

(defvar *stopwords* nil)

(defun ensure-stopword-hash ()
  (unless *stopwords*
    (setf *stopwords* (get-from-root '*stopwords*)))
  (unless *stopwords*
    (setf *stopwords* (make-hash-table :size 1000 :test 'equal ))))

(defun import-stopwords (file)
  (ensure-stopword-hash)
  (with-open-file (stream file)
    (with-transaction ()
      (do-stream-lines (line stream)
	(setf (gethash (normalize-string line) *stopwords*) t))))
  (add-to-root '*stopwords* *stopwords*))

(defun stopword-p (word)
  (gethash word *stopwords*))


;; Analyze data

(defparameter *word-counts* nil)

(defun message-word-counts ()
  (let ((hash (make-hash-table :size 100000 :test 'equal))
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

(defun incf-hash (key hash)
  (unless (gethash key hash)
    (setf (gethash key hash) 0))
  (incf (gethash key hash)))

(defun count-words (text hash)
  (let ((text-size (length text)))
    (labels ((find-string (start &aux pos)
	       (while (and (< start text-size)
			   (not (alphanumericp (char text start))))
		 (incf start))
	       (setf pos start)
	       (while (and (< pos text-size)
			   (alphanumericp (char text pos)))
		 (incf pos))
	       (when (> pos text-size)
		 (return-from count-words))
	       (let ((str (ref-string start (- pos start))))
		 (unless (stopword-p (nstring-downcase str))
		   (incf-hash str hash)))
	       (find-string (1+ pos)))
	     (ref-string (start length)
	       (make-array length :displaced-to text :displaced-index-offset start 
			   :element-type 'character)))
    (find-string 0))))

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
    