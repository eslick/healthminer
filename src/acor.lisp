(in-package :smart)

(defparameter *acor-db* nil)

(defun open-acor-db (&optional (dir '(:BDB "/Users/eslick/Work/db/acor/")))
  (let ((*store-controller* nil))
    (declare (special *store-controller*))
    (setf *acor-db* (open-store dir))))

;; ======================================
;; Schemas
;; ======================================

(defpclass email-account ()
  ((name :accessor user-name :initarg :name)
   (org :accessor user-org :initarg :org)
   (email :accessor user-email :initarg :email :index t)))

(defun get-account (email)
  (get-instance-by-value 'email-account 'email email))

(defun maybe-make-account (header &optional org)
  (ppcre:register-groups-bind (name email)
      ("(.*) \\<(.*)\\>" header)
;;    (print (cons name email))
    (aif-ret (get-account email)
      (make-instance 'email-account :name name :email email :org org))))

(defpclass email (message)
  ((dataset :accessor dataset-name :initarg nil)
   (in-reply-to :accessor reply-to :initarg :in-reply-to :initform nil)
   (headers :accessor headers :initarg :headers :initform nil)
   (fwd :accessor fwd-text :initarg :fwd)
   ;; Base class overrides
   (url :accessor relative-url :initarg :url :initform nil)
   (udate :accessor udate :derived-fn (lambda (inst)
					(values (parse-message-time (date inst)) t))
	  :slot-deps (date))))

(defmethod print-object ((email email) stream)
  (with-slots (date sender subject body) email
    (format stream "#<EMAIL \"~A\", \"~A\">"
	    (when sender (user-email sender)) date)))

(defun maybe-make-email (file headers &optional body remainder)
  "Doesn't include in-reply-to and src"
  (let* ((date (get-header "Date" headers))
	 (user (get-header "From" headers))
	 (org (get-header "Organization" headers))
	 (subject (get-header "Subject" headers))
	 (reply-to (get-header "In-Reply-To" headers))
	 (udate (handler-case (parse-message-time date)
		  (parse-time-error () 0))))
    (with-transaction ()
      (aif-ret (and (> udate 0) (get-instance-by-value 'email 'udate udate))
	(make-instance 'email
		       :src file
		       :headers headers
		       :date date
		       :subject subject
		       :sender (maybe-make-account user org)
		       :in-reply-to reply-to
		       :body body
		       :fwd remainder)))))

(defmethod message-sentences ((message email) &optional (size 2))
  (flatten
   (extract-sentence-windows (vector-tag (or (body message) "")) size)))

;; =======================================
;; DB ops
;; =======================================

;; ===============================================
;; Parser
;; ===============================================

(defun parse-acor-collection (directory prefix &optional skip-to)
  (let ((files (get-acor-files directory prefix)))
    (when skip-to (setf files (skip-to-file files skip-to)))
    (mapc #'parse-acor-file files)))

(defun skip-to-file (files skip-to)
  (awhen (position skip-to files :test #'equalp :key #'namestring)
    (nthcdr it files)))

(defun get-acor-files (directory prefix)
  (directory (make-pathname :directory directory 
			    :name prefix
			    :type "LOG*")
	     :directories nil))

(defun parse-acor-file (filename &aux results);; errors)
  (with-open-file (stream filename :direction :input)
    (loop 
       do (handler-case 
	      (let ((email (parse-acor-message filename stream)))
		(setf (email- email) filename)
		(push email results))
	    (end-of-file () (return results)))
;;	    (error () (if (> (length errors) 2)
;;			  (error "Too many bad messages in ~A near ~A" filename errors)
;;			  (push (file-position stream) errors))))
       finally (return results))))

(defun parse-acor-message (file stream)
  (find-message-start stream t)
  (let* ((headers (parse-email-headers stream))
	 (body (parse-body stream))
	 (rem (parse-remainder stream)))
    (let ((email (maybe-make-email file headers body rem)))
      (print email)
      email)))

(defun parse-body (stream)
  (let ((start (file-position stream)))
    (find-fwd-start stream)
    (let* ((end (file-position stream))
	   (length (1+ (- end start))))
      (file-position stream start)
      (let ((body (make-string length)))
	(read-sequence body stream :start 0 :end length)
	body))))

(defun parse-remainder (stream)
  (let ((start (file-position stream)))
    (find-message-start stream nil)
    (let* ((end (file-position stream))
	   (length (1+ (- end start))))
      (file-position stream start)
      (let ((remainder (make-string length)))
	(read-sequence remainder stream :start 0 :end length)
	remainder))))

(defun find-fwd-start (stream &optional consume-marker)
  (loop 
     for line = (read-line stream t)
     when (or (ppcre:scan "On ((Mon)|(Tue)|(Wed)|(Thu)|(Fri)|(Sat)|(Sun))" line)
	      (ppcre:scan "-+ Original Message -+" line)
	      (ppcre:scan "In a message dated" line)
	      (ppcre:scan "-+ .*@.* wrote:" line)
	      (ppcre:scan "\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*" line))
     do (progn 
	  (if consume-marker t (unread-line stream line))
	  (return line))))

(defun find-message-start (stream &optional consume-marker)
  (loop 
     for line = (read-line stream t)
     when (cl-ppcre:scan "==================" line)
     do (progn 
	  (if consume-marker t (unread-line stream line))
	  (return line))))

(defun unread-line (stream line)
  (file-position stream (- (file-position stream) (length line) 1)))

;; =========================================
;; Headers
;; =========================================

(defparameter *last-header-key* "Content-Transfer-Encoding")

(defun get-header (key headers)
  (assoc-get key headers #'equal))

(defun parse-email-headers (stream)
  (loop 
     for header = (parse-email-header stream)
     while header
     when (consp header)
     collect header))

(defun parse-email-header (stream)
  (let ((line (read-line stream)))
    (unless (empty-line-p line)
      (or (ppcre:register-groups-bind (key value)
	      ("(.[^:]*): (.*)" line)
	    (cons key (string-trim-whitespace value)))
	  t))))

(defun empty-line-p (line)
  (or (= (length line) 0)
      (and (< (length line) 2)
	   (let ((first (char line 0)))
	     (or (eq first #\Return)
		 (eq first #\Newline))))))

