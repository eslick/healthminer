(in-package :smart)

;; ==========================================================
;; SERVER

(defvar *lam-ui-acceptor* nil)

(defun start-lam-ui ()
  (when *lam-ui-acceptor* 
    (stop-lam-ui))
  (setq *lam-ui-acceptor* 
	(make-instance 'acceptor :port 8090 
		       :request-dispatcher 'lam-ui-dispatcher))
  (setf hunchentoot:*show-lisp-errors-p* t)
  (start *lam-ui-acceptor*))

(defun stop-lam-ui ()
  (stop *lam-ui-acceptor*)
  (setf *lam-ui-acceptor* nil))

;; ===========================================================
;; CONTENT PAGES

(defparameter *stream* nil)

(defmacro with-page ((&key title includes) 
		     &rest body)
  `(cl-who:with-html-output-to-string (*stream*)
     (:html
      (:head (:title ,title)
	     ,@(html-header-includes includes))
      (:body ,@body))))

(defmacro with-html (&body body)
  `(cl-who:with-html-output (*stream* *stream*)
     ,@body))

(defun html-header-includes (includes)
  (loop for include in includes collect
       (dbind (type path) include
	 (cond ((eq type :script)
		`(:script :src ,path))
	       ((eq type :stylesheet)
		`(:link :rel ,path))
	       (t (error "Unknown header include ~A" include))))))

(defun lam-ui-dispatcher (request)
  (serve-lam-ui-home))

(defun serve-lam-ui-home ()
  (handler-bind ((error (lambda (e) 
			  (funcall *debugger-hook* e nil))))
    (with-page (:title "LAM Listserv Demo Interface")
      (:h1 "LAM UI Demo")
      (render-tab-links)
      (render-current-tab))))

(defun home-tab ()
  (with-html
    (:div (:p "This is a test home page"))))

;; ==============================================================
;; OBJECTS

(defgeneric render-object (object)
  (:method ((fn function)) (funcall fn))
  (:method ((fn symbol)) (funcall fn)))

(defmethod render-object 

;; =============================================================      
;; TABS

(defclass simple-tab ()
  ((ref       :accessor tab-ref :initarg :ref)
   (link-text :accessor tab-link-text :initarg :link-text)
   (object    :accessor tab-object :initarg :object)))

(defparameter *tabs* 
  (list (make-instance 'simple-tab :ref "home" 
		       :link-text "Home Tab"
		       :object 'home-tab)
	(make-instance 'simple-tab :ref "test" 
		       :link-text "Test Tab"
		       :object 'test-tab)))

(defun get-tab (name)
  "tab = '(name . [function-name or object])"
  (awhen (find name *tabs* :key #'tab-ref :test #'equal)
    it))

(defun render-tab-links ()
  (mapc 'render-tab-link *tabs*))

(defun render-tab-link (tab)
  (with-html
    (:div :class "tablinks"
	  (:a :href (format nil "~A/~A" (puri:uri-path (puri:parse-uri (request-uri*))) (tab-ref tab))
	      (cl-who:str (tab-link-text tab))))))

(defun render-current-tab ()
  (let ((uri 
	(name (or (subseq (request-uri*) (position  "tab") "home")))
    (aif (tab-object (get-tab name))
	 (render-object it)
	 (render-object (tab-object (get-tab "home"))))))

(defun test-tab ()
  (with-html
    (:div (:p "This is a test tab"))))

    

