(in-package :smart)

;; ==========================================================
;;  Message definitions
;; ==========================================================

(defpclass message ()
  ((url :accessor relative-url :initarg :url :index t)
   (sender :accessor sender :initarg :sender :index t)
   (subject :accessor subject :initarg :subject)
   (date :accessor date :initarg :date :index t :inherit t)
   (udate :accessor udate :derived-fn (lambda (inst)
					(values (parse-message-time (date inst)) t))
	  :slot-deps (date)
	  :inherit t)
   (body :accessor body :initarg :body)
   (calais :accessor calais :initarg :calais :initform nil)))
   
(defparameter *print-message-body* nil)

(defmethod print-object ((msg message) stream)
  (with-slots (sender subject body) msg
    (format stream "#<LAM-MSG \"~A\", \"~A\">"
	    sender (when (slot-boundp msg 'date) (date msg)))))

(defmethod print-message-header (msg &optional (stream t))
  (with-slots (date sender subject body) msg
    (format stream "Date: ~A~%From: ~A~%Subject: ~A~%~%"
	    date sender subject)))

(defmethod print-message-body (msg &optional (stream t))
  (print (body (get-message msg)) stream))

(defun get-message-by-url (url)
  (get-instance-by-value 'lam-message 'url url))

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
    (ele:with-transaction (:txn-nowait t :txn-nosync t)
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
    (ele:with-transaction (:txn-nowait t :txn-nosync t)
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

(define-condition parse-time-error ()
  ((string :initarg :string :accessor time-string)))

(defun parse-message-time (string)
  (if (null string)
      (signal 'parse-time-error :string string)
      (parse-time-string string  *message-recognizers*)))

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
  (aif (or force (not *message-map*))
       (build-message-map)
       *message-map*))

(defun build-message-map (&optional (class 'message))
  (let ((hash (make-hash-table))
	(count 0))
    (setf *message-map* hash)
    (map-class 
     (lambda (message)
       (with-slots (body) message
	 (let ((vdoc (langutils::vector-tag (or body ""))))
	   (setf (gethash (get-message-id message) hash)
		 (list (mapcar #'get-lemma-for-id (vector-document-words vdoc))
		       vdoc
		       (get-extended-chunks vdoc)))))
       (when (= 0 (mod (incf count) 100))
	 (print count)))
     class)
    hash))

;; LDA topic vector caching 

(defun insert-message-topics (messages filename)
  (with-lda-reader (reader filename)
    (loop for message in messages 
       do (set-message-lda-topics message reader))))

(defun set-message-lda-topics (message reader)
  (let ((mrec (get-message-rec message)))
    (when (< (length mrec) 4)
      (setf (nthcdr 3 mrec) (cons nil nil)))
    (setf (fourth mrec)
	  (make-lda-message-annotation-vector message (funcall reader)))))

(defun make-lda-vector (message start end)
  (ignore-errors (subseq (message-topics message) start (1+ end))))

(defun chunk-has-topic (message chunk topic)
  (ignore-errors
    (let ((topics (message-topics message)))
      (member topic (array->list 
		     (subseq topics
			     (phrase-start chunk)
			     (min (1+ (phrase-end chunk))
				  (length topics))))))))

(defun topic-distribution (message &optional topic-labels)
  (let* ((topics (message-topics message))
	 (total (length topics)))
    (mapcar (f (entry)
	      (cons (aif-ret (and topic-labels (gethash (car entry) topic-labels))
		      (car entry))
		    (coerce (/ (cdr entry) total) 'float)))
	    (rest (sort (histogram 
			 (array->list 
			  (message-topics message)))
			#'>
			:key #'cdr)))))

(defun windows-for-topic (message topic &key (size 7) ids)
  (let ((terms (message-words message)))
    (loop 
       with last-pos = nil 
       for pos in (topic-positions message topic)
       when (or (not last-pos) (< last-pos (+ pos (/ size 2))))
       collect (prog1 
		   (let ((window (position-window pos size terms)))
		     (if ids window
			 (tokens-for-ids window)))
		 (setf last-pos pos)))))
  

(defun topic-positions (message topic)
  (loop for i from 0
        for top across (message-topics message)
        when (eq top topic)
        collect i))

;; Chunk caching

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
(defun mmap-topics (rec) (fourth rec))

(defun get-message-rec (msg)
  (typecase msg
    (number (gethash msg *message-map*))
    (message (gethash (get-message-id msg) *message-map*))))

(defun message-words (msg-ref) (mmap-words (get-message-rec msg-ref)))
(defun message-lemmas (msg-ref) (mmap-lemmas (get-message-rec msg-ref)))
(defun message-doc (msg-ref) (mmap-vdoc (get-message-rec msg-ref)))
(defun message-chunks (msg-ref) (mmap-chunks (get-message-rec msg-ref)))
(defun message-topics (msg-ref) (mmap-topics (get-message-rec msg-ref)))

;;;; ==========================================
;;;; UTILITY MACROS
;;;; ==========================================

(defmacro with-message-parts (names page &body body)
  (with-gensyms (pageval)
    `(let ((,pageval ,page))
       (let (,@(generate-header-extractors names pageval))
	 (let ((body ,(generate-extract-body pageval)))
	   ,@body)))))

;;;; ==========================================
;;;; ANNOTATIONS
;;;; ==========================================

(defparameter *msg-annotation-version* 2)

(defpclass msg-annotation ()
  ((message :accessor message :initarg :message :index t)
   (type :accessor type :initarg :type)
   (range :accessor range :initarg :range
	  :documentation "nil means all sentences, or 1 sentence or (s,e)")
   (version :accessor version :initarg :version 
	    :initform *msg-annotation-version*)))

(defmethod print-object ((anno msg-annotation) stream)
  (format stream "#<MSG-ANNO '~A'>" (type anno)))

(defun make-msg-annotation (message type &optional range notes)
  (ensure-transaction ()
    (make-instance 'msg-annotation
		   :message message
		   :type type
		   :range range
		   :notes notes)))

(defun msg-annotations (message)
  (get-instances-by-value 'msg-annotation 'message message))

(defun msg-annotations-by-type (type)
  (remove-nulls
   (map-class (lambda (anno)
		(when (eq (type anno) type)
		  anno))
	      'msg-annotation
	      :collect t)))

(defpclass text-annotation ()
  ((message :accessor message :initarg :message :index t)
   (type :accessor type :initarg :type :initform :none)
   (start :accessor start :initarg :start)
   (end :accessor end :initarg :end)
   (version :accessor version :initarg :version 
	    :initform *msg-annotation-version*)))

(defmethod print-object ((anno text-annotation) stream)
  (format stream "#<TEXT-ANNO '~A'>" (type anno)))

(defpclass text-annotation-archive (text-annotation)
  ())

(defmethod print-object ((anno text-annotation-archive) stream)
  (format stream "#<TEXT-ANARCH '~A'>" (type anno)))

(defun archive-text-annotations (annotations)
  (loop for annotation in annotations do
       (change-class annotation 'text-annotation-archive)))

(defun make-text-annotation (message type start end)
  (make-instance 'text-annotation
		 :message message
		 :start start 
		 :end end 
		 :type type))

(defun text-annotation->phrase (annotation &optional (expansion 0))
  (let ((doc (message-doc (get-message (message annotation)))))
    (make-instance 'phrase 
		   :document doc
		   :type (type annotation)
		   :start (max (- (start annotation) expansion) 0)
		   :end (min (+ (end annotation) expansion) 
			     (1- (length (document-text doc)))))))

(defun text-annotation->string (annotation &optional (expansion 0) with-tags)
  (phrase->string (text-annotation->phrase annotation expansion) :with-tags with-tags))

(defun print-text-annotation (annotation &key with-tags (expansion 0))
  (format t "~A: ~A~%"
	  (type annotation)
	  (text-annotation->string annotation expansion with-tags)))

(defmethod get-lda-vector ((anno text-annotation) &key &allow-other-keys)
  (make-lda-vector (message anno)
		   (start anno)
		   (end anno)))

(defmethod get-lda-vector ((phrase phrase) &key (message nil))
  (make-lda-vector message (phrase-start phrase) (phrase-end phrase)))

(defun text-annotations (message)
  (get-instances-by-value 'text-annotation 'message message))

(defun all-text-annotations ()
  (get-instances-by-class 'text-annotation))

(defun all-text-annotations-by-type (type)
  (select-if (f (anno) (eq type (type anno)))
	     (all-text-annotations)))

(defun all-text-annotated-messages ()
  (remove-duplicates
   (remove-nulls 
    (map-class #'message 'text-annotation :collect t))))

(defun review-annotation-type (type &optional (expansion 0))
  (mapcar (f (anno) 
	    (print-text-annotation anno :expansion expansion)
	    (format t "~%"))
	  (all-text-annotations-by-type type)))

;;
;; Annotation patterns
;;

(defun find-type-pairs ()
  (let ((msgs (all-text-annotated-messages)))
    (loop for msg in msgs collect
	 (select-if (f (anno) (member (type anno) '(symptom intervention)))
		    (text-annotations msg)))))

;;
;; Interactive annotation
;;

(define-condition repeat-annotation ()
  ())

(defun annotate-messages (messages &key skip start-at)
  (labels ((skip-to (msgref)
	     (subseq messages (position (get-message msgref) messages 
					:key #'get-message))))
    (when start-at
      (setf messages (skip-to start-at)))
    (loop for message in messages do
	 (handler-case
	     (unless (and skip (has-annotations? message))
	       (annotate-message message))
	   (repeat-annotation ()
	     (annotate-messages (skip-to message))))))
  t)

(defun has-annotations? (message)
  (or (msg-annotations message)
      (text-annotations message)))

(defmacro while-annotating ((expr) &body options)
  (assert (find 'return (flatten options)))
  `(loop
      (let ((,expr (progn (princ #\>) (princ #\Space) (read))))
	(cond ,@options))))

(defun annotate-message (message)
  ;; Message level annotations
  (let ((sentences (flatten (extract-sentence-windows (message-doc message) 2))))
    ;; Print all sentences w/ #'s
    (format t "~A~%" (get-message-id message))
    (loop for sentence in sentences 
	 for i from 0 do
	 (format t "~A: ~A~%" i (phrase->string sentence)))
    (loop for annotation in (msg-annotations message) do
	 (with-slots (type range) annotation
	   (format t "(~A ~A)~%" type range)))
    ;; Annotate sentences or regions of sentences
    ;; Default is message label of all sentences
    (while-annotating (expr)
      ((eq expr 'c) (return))
      ((eq expr 'r) (signal 'repeat-annotation))
      ((eq expr 'x) (return-from annotate-message))
      ((eq expr 'd) (drop-instances (msg-annotations message)))
      ((symbolp expr) 
       (make-msg-annotation message expr))
      ((listp expr)
       (apply #'make-msg-annotation message expr)))
    ;; Phrase level annotations
    (loop for sentence in sentences do
	 (print-sentence message sentence :marks t :annotations t)
	 (while-annotating (expr)
	   ((eq expr 'd)
	    (drop-instances (annotations-for-phrase message sentence)))
	   ((eq expr 'r) 
	    (signal 'repeat-annotation))
	   ((eq expr 'x)
	    (return-from annotate-message))
	   ((listp expr)
	    (dbind (type start &optional end) expr
	      (when (null end)
		(setf end start))
	      (make-text-annotation message type 
				    (+ (phrase-start sentence) start)
				    (+ (phrase-start sentence) end))))
	   (t (return))))))

(defun print-sentence (message sphrase &key marks annotations)
  (let* ((words (tokens-for-ids (phrase-words sphrase))))
    (aif (and annotations (annotations-for-phrase message sphrase))
      (loop for annotation in it do
	   (with-slots (start end type) annotation
	     (format t "~A:~A ~A~%" 
		     (- start (phrase-start sphrase))
		     (- end (phrase-start sphrase))
		     type)))
      (format t "~%"))
    (when marks
      (loop 
	 for i from 0 
	 for word in words do
	   (format t "~v,A " (max (length word) 3) i))
      (format t "~%"))
    (loop for word in words do
	 (format t "~v,A " (max (length word) 3) word))
    (format t "~%")))

(defun annotations-for-phrase (message sphrase &optional filter)
  (let ((start (phrase-start sphrase))
	(end (phrase-end sphrase)))
    (ele:with-transaction ()
      (select-if (f_ (and (>= (start _) start)
			  (<= (end _) end)))
		 (let ((annos (text-annotations message)))
		   (if filter
		       (filter-annotations filter annos)
		       annos))))))

(defun review-annotations (messages)
  (loop for message in messages
     for annos = (text-annotations message)
     when annos 
     do (progn (print (msg-annotations message))
	       (print-message-body message)
	       (mapc #'print-text-annotation annos))))


;; ==============================
;;  OpenCalais
;; ==============================

(defparameter *calais-license-key* "nf4c5sc56xczyax4d6syzr3t")
(defparameter *calais-params*
"<c:params xmlns:c=\"http://s.opencalais.com/1/pred/\" xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">

  <c:processingDirectives c:contentType=\"text/txt\" c:enableMetadataType=\"GenericRelations,SocialTags\" c:outputFormat=\"application/json\" c:docRDFaccesible=\"true\" >
  </c:processingDirectives>

  <c:userDirectives c:allowDistribution=\"true\" c:allowSearch=\"true\" c:externalID=\"ianeslick\" c:submitter=\"ABC\">
  </c:userDirectives>

  <c:externalMetadata>
  </c:externalMetadata>

</c:params>")

(defun open-calais-message (msg &optional force)
  (let ((*use-strict-json-rules* nil))
    (declare (special *use-strict-json-rules*))
    (if (or force (not (calais msg)))
	(setf (calais msg) (open-calais (body msg)))
	(calais msg))))

(defun open-calais (content)
  (ignore-errors 
    (let ((stream (drakma:http-request "http://api.opencalais.com/enlighten/rest/"
				     :parameters 
				     `(("licenseID" . ,*calais-license-key*)
				       ("content" . ,content)
				       ("paramsXML" . ,*calais-params*))
				     :want-stream t)))
      (unwind-protect (json::decode-json stream)
	(close stream)))))

(defun calais-type-group-entries (response group)
  (select-if (lambda (entry)
	       (equal (assoc-get :--type-group (rest entry)) group))
	     (rest response)))

(defun calais-msg-types (message)
  (awhen (calais-type-group-entries (calais message) "topics")
    (mapcar (lambda (entry)
	      (assoc-get :category-name (rest entry)))
	    it)))

(defun calais-health-type-p (message)
  (member "Health_Medical_Pharma"
	  (calais-msg-types message)
	  :test #'equal))

(defun calais-entities (message)
  (awhen (calais-type-group-entries (calais message))
    nil))


;; ==========================
;; ASpell
;; ==========================

(defstruct aspell-process 
  input output procid)

;;(defparameter *aspell-process*)
	   
;; ==========================
;; UMLS quick dict
;; ==========================


(defvar *umls-dictionary* (make-hash-table :test #'equal))
(defvar *umls-term-dictionary* (make-hash-table))

(defun lookup-umls-term-count (term)
  (gethash (id-for-token term) *umls-term-dictionary*))

(defun umls-term-matches (terms)
  (loop for term in terms
     when (lookup-umls-term-count term)
     collect (if (stringp term)
		 term
		 (token-for-id term))))

(defun count-umls-term-matches (terms)
  (let ((mcount (length (umls-term-matches terms)))
	(tcount (length terms)))
    (values mcount (coerce (/ mcount tcount) 'float) tcount)))

(defun quick-import-umls-data (file)
  (with-open-file (stream file :direction :input)
    ;; headers
    (read-line stream) 
    (loop 
       for line = (read-line stream nil nil)
       for i from 0
       for fullspec = (parse-umls-fullyspecified-words line)
       while fullspec
       do 
	 (setf (gethash fullspec *umls-dictionary*) t)
	 (loop for word in (extract-words fullspec)
	    for i from 0 do
	      (unless (stopword-p word)
		(incf-hash (id-for-token word) *umls-term-dictionary*)))
       when (= 0 (mod i 10000)) do (print i))))

(defun parse-umls-fullyspecified-words (entry)
  (awhen (third (parse-umls entry))
    (string-downcase it)))

(defun parse-umls (entry)		    
  (split-sequence:split-sequence #\Tab entry))

;;;;
;;;; Verb and topic distributions
;;;;

(defun nouns+topics (message &optional label-table)
  (pos+topics message label-table 'noun-pos-p))

(defun verbs+topics (message &optional label-table)
  (pos+topics message label-table 'verb-pos-p))

(defun pos+topics (message label-table &optional (pred 'verb-pos-p) get-label)
  (loop 
     with text = (document-text (message-doc message))
     with topics = (message-topics message)
     for offset in (pospred-locations message (cond ((functionp pred) pred)
						    ((ignore-errors (symbol-function pred)) pred)
						    ((symbolp pred)
						     (lambda (pos) (eq pos pred)))
						    (t (error "Can't select pos topics using ~A"
							      pred))))
     for word = (token-for-id (aref text offset))
     collect (cons word (get-topic-label (aref topics offset) label-table))))

(defun pospred-locations (message pospred)
  (loop for i from 0 
     for pos across (document-tags (message-doc message))
     when (funcall pospred pos)
     collect i))


;;;
;;; Head verb/noun extraction test
;;;

;; Identify candidates based on head noun and leading head verb (when exists)
;; For a message, extract pairs of symptom/treatment (classify?)

;; Identify sign of influence
;; Identify direction of influence (->, <-, -) (inc. cause/assoc)


