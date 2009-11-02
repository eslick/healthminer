(in-package :smart)

(in-package :smart)

(defclass ps-thread ()
  ((:messages :initarg :messages :accessor messages)))

(defclass ps-message ()
 ((:title :initarg :title :accessor title)
  (:author :initarg :author :accessor author)
  (:date :initarg :date :accessor date)
  (:scrape-date :initarg :scrape-date :accessor scrape-date)
  (:contents :initarg :contents :accessor contents)
  (:num :initarg :num :accessor num)))

;; tools

(defparameter *page-root* "http://talkpsoriasis.org/")

(defun build-document (rel)
 (chtml:parse
  (drakma:http-request (talkp-url rel))
  (stp:make-builder)))

(defun build-document-ab (ab)
 (chtml:parse
  (drakma:http-request ab)
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

(defun talkp-url (fragment)
 (concatenate 'string *page-root* fragment))

(defun get-max-page (doc)
 (let ((max-page (stp-list-at-path doc "//body//table[1]//table//td[last()-1]/a")))
   (if max-page
	(parse-integer max-page)
	0)))

(defun clean-link-list (lst)
 (mapcar (compose #'strip-session-parameter #'stp:string-value) lst))

(defun clean-text (text)
 (mapcar (compose #'stp:string-value) text))

(defun parse-field (doc base path)
 (first (clean-text (stp-list-at-path doc (concatenate 'string base path)))))

;; mining

(defun get-forum-links ()
 (clean-link-list
  (stp-list-at-path (build-document "") "//body/table[4]//tr/td[3]//a/@href")))

(defun get-thread-links (forum-rel)
 (let* ((first-page (build-document forum-rel))
	 (max-page (get-max-page first-page))
	 links)
   (append (stp-list-at-path first-page "//body//table[@id='threadslist']//td[3]/div[1]/a[last()]/@href") links)
   (loop for pg from 2 to max-page do
	 (let* ((rel (concatenate 'string forum-rel "&page=" (write-to-string pg)))
		(page (build-document rel)))
	   (append (stp-list-at-path page "//body//table[@id='threadslist']//td[3]/div[1]/a[last()]/@href") links)))
   (clean-link-list links)))

(defun get-messages (thread-rel)
 (let* ((first-page (build-document thread-rel))
	 (max-page (get-max-page first-page))
	 (thread (make-instance 'ps-thread)))
   (parse-msg-page first-page thread)
   (loop for pg from 2 to max-page do
	 (let* ((rel (concatenate 'string thread-rel "&page=" (write-to-string pg))))
	   (parse-msg-page (build-document rel) thread)))
   thread))

(defun parse-msg-page (doc thread)
 (let ((post-ids (clean-text (stp-list-at-path doc "//body/div[@id='posts']//table/@id"))))
   (loop for id in post-ids collect
	 (let* ((base (concatenate 'string "//body/div[@id='posts']//table[@id='" id "']"))
		(msg (make-instance 'ps-message
				    :num (parse-field doc base "//div[@class='normal']/a[@id]")
				    :title (parse-field doc base "//div[@class='smallfont']/strong")
				    :author (parse-field doc base "//a[@class='bigusername']")
				    :date (parse-field doc base "//div[@class='normal'][2]")
				    :scrape-date (get-universal-time)
				    :contents (parse-field doc base "//tr[3]//div[@id]"))))
	   (push msg (messages thread))))))
