(in-package :smart)

(defclass forum-thread ()
  ((:messages :initarg :messages :accessor messages :initform nil)
   (:id :initarg :id :accessor id)))

(defclass forum-message ()
 ((:title :initarg :title :accessor title)
  (:author :initarg :author :accessor author)
  (:date :initarg :date :accessor date)
  (:scrape-date :initarg :scrape-date :accessor scrape-date)
  (:contents :initarg :contents :accessor contents)
  (:num :initarg :num :accessor num)
  (:thread :initarg :thread :accessor thread)))

;; tools

(defparameter *page-root* "http://talkpsoriasis.org/")

(defparameter *db-path* "/Users/lauren/tp-db/")

(defun build-document (rel)
 (chtml:parse
  (drakma:http-request (talkp-url rel))
  (stp:make-builder)))

(defun stp-list-at-path (doc path)
 "We run an xpath query over the stp document;
  all terms in the query are assumed to be xhtml (like body, div, p, etc).
  we then turn the node-set object into a list so it's easier to work with"
 (xpath:map-node-set->list
  #'identity
  (xpath:with-namespaces ((nil "http://www.w3.org/1999/xhtml"))
             (xpath:evaluate path doc))))

(defun strip-session-parameter (url)
 (cl-ppcre:regex-replace "s=[^&]*&" url ""))

(defun talkp-url (rel)
 (strcat *page-root* rel))

(defun get-max-page (doc)
 (let* ((max-page (first (clean-text (stp-list-at-path doc "//body//table[1]//td[2]//td[1][@class='vbmenu_control']")))))
   (if max-page
	(parse-integer (cl-ppcre:regex-replace "Page ([0-9])* of " max-page ""))
	1)))

(defun clean-links (lst)
 (mapcar (compose #'strip-session-parameter #'stp:string-value) lst))

(defun clean-text (text)
 (mapcar (compose #'string-trim-whitespace #'stp:string-value) text))

(defun parse-field (doc base path)
 (clean-text (stp-list-at-path doc (strcat base path))))

(defun strcat (&rest args)
 (let ((str ""))
   (loop for string in args do
	 (setq str (concatenate 'string str string))
	 finally (return str))))

(defun strip-newline (string)
 (while (search string #\newline) do
   (setq string (cl-ppcre:regex-replace #\newline string " ")))
 string)

;; mining

(defun get-forum-links ()
 (clean-links
  (stp-list-at-path (build-document "") "//body/table[4]//tr/td[3]//a/@href")))

(defun get-thread-links (forum-rel &optional (startpg 1) endpg)
 (if (not endpg)
     (setq endpg (get-max-page (build-document (strcat forum-rel "&page=" (write-to-string startpg))))))
 (loop for pg from startpg to endpg with links do
      (let* ((rel (strcat forum-rel "&page=" (write-to-string pg)))
	      (page (build-document rel)))
	 (setq links (append (clean-links (stp-list-at-path page "//body//table[@id='threadslist']//td[3]/div[1]/a[last()]/@href")) links)))
    finally (return links)))

(defun get-messages (thread-rel &optional (startpg 1) endpg)
 (let ((id (cl-ppcre:regex-replace "(.)*t=" thread-rel "")))
   (with-open-file (file (strcat *db-path* id ".sexp")
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists nil)
     (if file
	  (progn
	    (if (not endpg)
		(setq endpg (get-max-page (build-document (strcat thread-rel "&page=" (write-to-string startpg))))))
	    (loop for pg from startpg to endpg do
		 (let* ((rel (strcat thread-rel "&page=" (write-to-string pg))))
		   (parse-msg-page (build-document rel) file))))))))

(defun parse-msg-page (doc file)
 (let ((post-ids (clean-text (stp-list-at-path doc "//body/div[@id='posts']//table/@id"))))
   (loop for id in post-ids collect
	 (let* ((base (strcat "//body/div[@id='posts']//table[@id='" id "']"))
		(num (parse-field doc base "//div[@class='normal']/a[@id]"))
		(title (parse-field doc base "//div[@class='smallfont']/strong"))
		(author (parse-field doc base "//a[@class='bigusername']"))
		(date (parse-field doc base "//div[@class='normal'][2]"))
		(scrape-date (get-universal-time))
		(contents (parse-field doc base "//tr[3]//div[@id]")))
	   (prin1 (list num title author date scrape-date contents) file)))))

;; loading data

(defun load-all (&optional (dir *db-path*))
 (loop for file in (directory (strcat dir "*.sexp")) collect
      (load-thread (namestring file))))

(defun load-thread (filename)
 (with-open-file (file filename :direction :input)
   (let* ((id (cl-ppcre:regex-replace
		"(.)*(/)" 
		(cl-ppcre:regex-replace ".sexp" filename "")
		""))
	   (thread (make-instance 'forum-thread
				 :id id))
	   (msgs (load-messages file thread)))
     (setf (messages thread) msgs)
     thread)))

(defun load-messages (file thread)
 (loop for msg = (read file nil (list nil))
    until (equal msg (list nil)) collect
      (make-instance 'forum-message
		      :num (nth 0 msg)
		      :title (nth 1 msg)
		      :author (nth 2 msg)
		      :date (nth 3 msg)
		      :scrape-date (nth 4 msg)
		      :contents (nth 5 msg)
		      :thread thread)))

(defmethod next-message ((message forum-message))
 (let ((num (first (num message)))
	(thread (thread message)))
   (nth (+ 1 (parse-integer num)) (messages thread))))

;; preprocessing

(defun generate-raw (end &optional (start 0) (dir *db-path*))
 (with-open-file (raw (strcat dir "forum.raw") :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
   (loop for file in (directory (strcat dir "*.sexp"))
      for n from start to end do
	 (let ((thread (load-thread (namestring file))))
	   (princ (id thread) raw)
	   (princ " NA " raw)
	   (princ (get-tokens thread) raw)
	   (princ #\newline raw)))))

(defun get-tokens (thread)
 (loop for message in (messages thread) with strings do
      (multiple-value-bind (successp strlen tokenizedstr chunk)
	   (langutils:tokenize-string (car (contents message)))
	 (declare (ignore successp strlen chunk))
	 (push (strcat (strip-newline tokenizedstr) " ") strings))
      finally (return (apply #'strcat strings))))

;; causality utils

(defparameter *context-size* 40)

(defun make-chunk (n text)
 (loop for i from 0 to (- *context-size* 1) with sentences do
      (push (nth (+ n i) text) sentences)
      finally (return (apply #'strcat sentences))))

(defun every-other (lst)
 (loop for n from 1 to (- (length lst) 1) by 2 collect (nth n lst)))

(defun split-after (delim string)
 (let ((indices (cons 0 (every-other (cl-ppcre:all-matches delim string)))))
   (if (not (equal (length string) (car (last indices))))
	(pushlast (length string) indices))
   (loop for n from 0 to (- (length indices) 2) collect
	 (subseq string (nth n indices) (nth (+ n 1) indices)))))

(defmethod convert-msg ((msg forum-message))
 (let ((newmsg (make-instance 'message
			       :url (id (thread msg))
			       :sender (author msg)
			       :subject (title msg)
			       :date (date msg)
			       :body (contents msg))))
   newmsg))

(defparameter *context-size* 3)

(defun get-context (message t1 t2)
 (multiple-value-bind (successp length tokenized chunk)
     (langutils:tokenize-string (body message))
   (declare (ignore successp length chunk))
   (let ((text (split-after "[.?!]" tokenized)))
     (loop for n from 0 to (- (length text) *context-size*) with context do
	   (let ((chunk (make-chunk n text)))
	     (if (and (search t1 chunk) (search t2 (second (cl-ppcre:split t1 chunk))))
		 (setq context (cl-ppcre:split (strcat t1 "|" t2) chunk))))
	   finally (return context)))))

(defun contexts-in-threads (threadlist t1 t2)
 (loop for thread in threadlist with contexts do
      (loop for message in (messages thread) do
	    (aif (get-context (convert-msg message) t1 t2)
		 (push it contexts)))
    finally (return contexts)))