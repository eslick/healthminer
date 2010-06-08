(in-package :smart)

(defvar *api-acceptor* nil)

(defun start-data-api (&optional (port 4242))
  (unless *api-acceptor*
    (setf *api-acceptor* 
	  (make-instance 'acceptor 
			 :port port :address "localhost" 
			 :request-dispatcher 'smart-dispatcher)))
  (start *api-acceptor*))

(defun stop-data-api ()
  (assert *api-acceptor*)
  (stop *api-acceptor*)
  (setf *api-acceptor* nil))

;; ============================
;;  Dispatcher
;; ============================

(defvar *handlers* nil)

(defun find-data-handler (tokens)
  (awhen (find tokens *handlers* :test #'equalp :key #'car)
    (cdr it)))

(eval-when (:compile-toplevel :load-toplevel)
  (defun add-data-handler (tokens handler)
    (remove-data-handler tokens)
    (setf *handlers* (cons (cons tokens handler) *handlers*))))

(defun remove-data-handler (tokens)
  (setf *handlers* (remove tokens *handlers* :test #'equalp :key #'car)))

(defun smart-dispatcher (request)
  (let ((tokens (split-uri-path (request-uri request))))
    (aif (and (equal (first tokens) "data") (find-data-handler (rest tokens)))
	 (funcall it (request-method*) (get-parameters*) (raw-post-data))
	 (progn (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
		(hunchentoot:abort-request-handler "API Command Not Found")))))


;; ============================
;;  JSON Handlers
;; ============================

(eval-when (:compile-toplevel :load-toplevel)
  (defun split-uri-path (uri)
    (let ((end (or (position #\? uri) (length uri))))
      (remove-if #'is-string-empty
		 (split-sequence:split-sequence #\/ (subseq uri 0 end)))))

  (defun wrap-data-handler (fn)
    "Takes a function that sends/receives JSON compatible lisp structures and
   handles the conversion into a proper hunchentoot request handler"
    (lambda (method params data)
      (wrap-data-handler* fn method params data))))

(defun wrap-data-handler* (fn method params data)
    (let ((jdata (ignore-errors
		   (json:decode-json-from-string data))))
      (let ((result (funcall fn method (convert-params params) jdata)))
	(setf (content-type*) "text/json") ;; text/plain?
	(json:encode-json-to-string result))))

(defun convert-params (params)
  (mapcar (lambda (param)
	    (dbind (key . value) param
	      (cons (intern (string-upcase key) :keyword) 
		    (convert-value value))))
	  params))

(defun convert-value (value)
  (if (= (length value) 0) ""
      (let ((lisp-value (read-from-string value)))
	(if (numberp lisp-value)
	    lisp-value
	    value))))
    

(defmacro define-api-handler (prefix args &body body)
  `(progn
     (add-data-handler ',(split-uri-path prefix)
       (wrap-data-handler
	(lambda ,args
	  (declare (ignorable ,@args))
	  ,@body)))))

;; =============================
;;  Tests
;; =============================

(define-api-handler "/wordlattice/test1" (method params json)
  '(((:ID . 0) (:NAME . "my") (:TYPE . "words") (:CHILDREN 1 2)) ((:ID . 1) (:NAME . "name is") (:TYPE . "words") (:CHILDREN 3 4)) ((:ID . 2) (:NAME . "favorite book is Armageddon") (:CHILDREN)) ((:ID . 3) (:NAME . "Happy") (:TYPE . "words") (:CHILDREN)) ((:ID . 4) (:NAME . "Foo Bar") (:TYPE . "words") (:CHILDREN))))

