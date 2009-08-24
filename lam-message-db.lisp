(in-package :smart)

;; ==========================================================
;;  Message definitions
;; ==========================================================

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
    (format stream "#<LAM-MSG \"~A\", \"~A\">"
	    sender date)))

(defmethod print-message-header (msg &optional (stream t))
  (with-slots (date sender subject body) msg
    (format stream "Date: ~A~%From: ~A~%Subject: ~A~%~%"
	    date sender subject)))

(defmethod print-message-body (msg &optional (stream t))
  (print (body (get-message msg)) stream))

(defun get-message-by-url (url)
  (get-instance-by-value 'message 'url url))

(defun rebuild-index (class indexed-slot)
  (let ((index (find-inverted-index class indexed-slot)))
    (map-btree (lambda (k v)
		 (remove-current-kv))
	       index)
    (map-class (lambda (inst)
		 (setf (get-value (slot-value inst indexed-slot) index) (elephant::oid inst)))
	       class)))

;;
;; ==========================================================
;; Listserv Scraping
;; ==========================================================
;;

(defparameter site-base-url "https://listserv.uc.edu")

(defparameter monthly-archive-link-template
  "/cgi-bin/wa.exe?A1=indYYMM&L=LAM-PATIENTS")

(defparameter monthly-archive-start-date '(January . 2006))
(defparameter monthly-archive-end-date '(June . 2009))

(defparameter month-key 
  '((january . 1)
    (february . 2)
    (march . 3)
    (april . 4)
    (may . 5)
    (june . 6)
    (july . 7)
    (august . 8)
    (september . 9)
    (october . 10)
    (november . 11)
    (december . 12)))

(defun next-month-pair (pair)
  (if (eq (first pair) 'december)
      (cons 'january (1+ (cdr pair)))
      (cons (car (second (member (first pair) month-key :key #'car)))
	    (cdr pair))))

(defun monthly-archive-links ()
  (let ((month monthly-archive-start-date))
    (loop 
     do (setq month (next-month-pair month))
     while (not (equal month monthly-archive-end-date))
     collect (list (ppcre:regex-replace "YY" 
		    (ppcre:regex-replace "MM"
		     (copy-seq monthly-archive-link-template)
		     (format nil "~2,'0D" (cdr (assoc (first month) month-key))))
		    (format nil "~2,'0D" (- (cdr month) 2000)))
		   month))))


;;
;; Fetch pages
;;

(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))

(defun get-main-page ()
  (fetch-relative-url "/archives/lam-patients.html"))

(defun fetch-relative-url (rel)
  (fetch-html-page (strcat site-base-url rel)))

(defun fetch-html-page (url)
  (drakma:http-request url :user-agent "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)"))

(defun get-archive-pages ()
  (loop for (url ref) in (monthly-archive-links)
       collecting
       (aif-ret (lookup-archive-page ref)
		(let ((page (fetch-relative-url url)))
		  (format t "Fetched: ~A, ~A~%" (first ref) (cdr ref))
		  (add-to-root ref page :sc *store-controller*)
		  page))))

(defun lookup-archive-page (ref)
  (get-from-root ref :sc *store-controller* ))

;;
;; Extract message urls
;;

(defun all-register-groups (regex text &key sharedp &aux matches)
  (ppcre:do-register-groups (context) (regex text (nreverse matches) :sharedp sharedp)
    (push context matches)))

(defun get-all-urls (page)
  (all-register-groups "<a href=\"(.*)\">" page :sharedp t))

(defun select-message-urls (urls)
  (select-if (lambda (url) 
	       (search "&P=" url)) 
	     urls))

(defun normalize-urls (urls)
  (collect (lambda (url)
	     (ppcre:regex-replace "F=&S=&" url ""))
    urls))

(defun get-message-urls (archive-page)
  (normalize-urls
   (select-message-urls
    (get-all-urls archive-page))))

;;
;; Archive-all-messages (long running extraction process)
;;

(defun archive-all-messages ()
  (dolist (page (get-archive-pages))
    #+allegro (excl:gc t)
    (with-transaction (:txn-nowait t :txn-nosync t)
      (dolist (url (get-message-urls page))
	(archive-message url)))))

(defun archive-message (url)
  (handler-case 
      (aif (get-message-by-url url)
	   (progn nil)
;	     (format t "Already-have...~%")
;	     (print-message-header it))
	   (let* ((frame-page (fetch-relative-url url))
		  (body (get-body-content frame-page)))
	     (print-message-header 
	      (create-message url frame-page body))))
    (error (e)
      (log-failed-url url (type-of e))))
  (finish-output))

(defun create-message (url frame-page body-text)
  (when (< (length body-text) 10) 
    (error "Truncated message body for url: ~A" url))
  (destructuring-bind (date sender subject) 
      (extract-message-headers frame-page)
    (with-transaction (:txn-nowait t :txn-nosync t)
      (make-instance 'message
		     :url url
		     :date date
		     :sender sender
		     :subject subject
		     :body body-text))))

;; Headers

(defun extract-message-headers (frame)
  (ppcre:register-groups-bind (date) ("Date:.*<tt>(.*)</tt>" frame)
    (ppcre:register-groups-bind (sender) ("From:.*<tt>(.*)</tt>" frame)
      (ppcre:register-groups-bind (subject) ("Subject:.*<tt>(.*)</tt>" frame)
	(list date 
	      (trim-sender sender)
	      (trim-non-alphanumeric subject))))))

(defun trim-sender (string)
  (trim-non-alphanumeric (ppcre:regex-replace "\\&\\#60.*62" string "")))
	  

;; Body content

(defun get-body-content (frame-page)
  (let* ((body-page (get-body-page frame-page))
	 (end (when body-page (get-body-end body-page))))
    (unless body-page
      (error "No body found"))
    (collapse-whitespace
     (strip-raw-html-regex 
      (subseq body-page 24 end)))))

(defun get-body-end (body-page)
  (apply #'min 
	 (1- (length body-page))
	 (remove-nulls 
	  (list (ppcre:scan "<BLOCKQUOTE" body-page)
		(ppcre:scan "----- Original Message -----" body-page)
		(ppcre:scan "In a message dated" body-page)
		(ppcre:scan "On ((Mon)|(Tue)|(Wed)|(Thu)|(Fri)|(Sat)|(Sun))" body-page)))))

(defun get-body-page (frame-page)
  (fetch-relative-url 
   (string-right-trim-one-char 
    #\" 
    (subseq (ppcre:scan-to-strings "<iframe src=\".*\"" frame-page) 13))))

;;
;; URL failure trace
;;

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

;;
;; Strip html markup
;;

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

;;
;; parse time string
;;

(defparameter *message-recognizers*
  (list (stdutils::make-fmt-recognizer "%a, %d %b %Y %H:%M:%S %Z")))

(defun parse-message-time (string)
  (parse-time-string string  *message-recognizers*))

;;
;; Extract message text
;;

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

;; ==========================================================
;;  Message working DB DSL
;; ==========================================================

(defun get-message (mid)
  (typecase mid
    (number (get-object mid))
    (t mid)))

(defun get-message-id (mref)
  (typecase mref
    (number mref)
    (t (ele::oid mref))))

;; DB Cached

(defvar *message-map* nil)

(defun get-message-map (&optional force)
  (aif-ret (and (not force) *message-map*)
    (build-message-map)))

(defun build-message-map ()
  (let ((hash (make-hash-table))
	(count 0))
    (map-class 
     (lambda (message)
       (with-slots (body) message
	 (let ((vdoc (langutils::vector-tag body)))
	   (setf (gethash (ele::oid message) hash)
		 (list (mapcar #'get-lemma-for-id (vector-document-words vdoc))
		       vdoc
		       (get-extended-chunks vdoc)))))
       (when (= 0 (mod (incf count) 100))
	 (print count)))
     'message)
    (setf *message-map* hash)))

(defun get-extended-chunks (doc)
  (append (get-nx-chunks doc)
	  (get-event-chunks doc)
	  (get-extended-event-chunks1 doc)
	  (get-extended-event-chunks2 doc)))

(defmacro map-messages ((k v &optional (report 0)) &body body)
  (assert (and (symbolp k) (symbolp v)))
  `(let ((results nil)
	 (count 0))
     (maphash (lambda (,k ,v)
		(declare (ignorable ,k ,v))
		(when (and (> ,report 0) (= (mod (incf count) ,report) 0))
		  (print count))
		(push (progn ,@body) results))
	      (get-message-map))
     results))

(defun messages-for-word (word)
  (let ((msgs nil)
	(id (get-lemma-for-id (id-for-token word))))
    (map-messages (mid rec)
      (when (member id (mmap-lemmas rec))
	(push mid msgs)))
    msgs))

(defun messages-for-words (words)
  (let ((ids (mapcar #'get-lemma-for-id words)))
    (loop for rec being the hash-value of (get-message-map)
       using (hash-key id)
       when (every (lambda (id)
		     (member id (mmap-lemmas rec)))
		   ids)
       collect id)))

(defun get-object (id)
  (ele::controller-recreate-instance *store-controller* id))

;; Extract component values

(defun mmap-string (rec) 
  (vector-document-string (mmap-vdoc rec)))
(defun mmap-lemmas (rec) (first rec))
(defun mmap-words (rec) (vector-document-words (mmap-vdoc rec)))
(defun mmap-vdoc (rec) (second rec))
(defun mmap-chunks (rec) (third rec))

(defun get-message-rec (msg)
  (typecase msg
    (number (gethash msg *message-map*))
    (message (gethash (ele::oid msg) *message-map*))))

(defun message-lemmas (msg-ref) (mmap-lemmas (get-message-rec msg-ref)))
(defun message-doc (msg-ref) (mmap-vdoc (get-message-rec msg-ref)))
(defun message-chunks (msg-ref) (mmap-chunks (get-message-rec msg-ref)))


;;;; ==========================================
;;;; UTILITY MACROS
;;;; ==========================================

(defmacro with-message-parts (names page &body body)
  (with-gensyms (pageval)
    `(let ((,pageval ,page))
       (let (,@(generate-header-extractors names pageval))
	 (let ((body ,(generate-extract-body pageval)))
	   ,@body)))))

