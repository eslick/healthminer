
(in-package :smart)

(defparameter *users* '(("eslick" "food" (:scary-hair :evil))))

(defvar *our-mutex* (hunchentoot-mp:make-lock "our-lock"))
(defvar *events* '())
(defvar *model* nil)
  
(defparameter *dispatch-list* nil)

(defun create-list-dispatcher ()
  (lambda (request)
    (loop for (fn scanner) in *dispatch-list*
       when (ppcre:scan scanner (script-name request))
       return fn)))

(defun launch-server ()
  (hunchentoot:start-server :port 4242)
  (setf *model* (make-instance 'survey-model 
			       :file "/Users/eslick/Work/newmed/smart/websurvey.csv"))
  (push (create-list-dispatcher)
	*dispatch-table*))

(defun add-regex-dispatch (regex fn)
  (let ((scanner (ppcre:create-scanner regex)))
    (pushnew (list fn scanner) *dispatch-list* :key #'first)))

(defmacro defpage (name url &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (add-regex-dispatch ,(format nil "^/~a[/]*" url) ',name)))

(defmacro defapp (name url &body body)

(defmacro with-template (title &body body)
  `(with-html-output-to-string (*standard-output*)
     (:html
      (:head (:title (fmt "Graph demo - ~a" ,title)))
      (:body (:h1 "Graph demo")
	     (:div (:a :href "/" "Main") " - "
		   (:a :href "/events" "Events") " - "
		   (:a :href "/foo" "Foo"))
	     ,@body))))

;; Main Page - test datasets

(defpage main-page "" 
  (with-template "Main Demo"
    (:p "This is a simple test")
    (:form :action "https://lamsight-dev.media.mit.edu/servlets-examples/servlet/SimpleHello5" 
	   :method "post"
	   (:label "Foo")
	   (:select :name "json"
		    (loop for i from 6 upto 8 do
			 (htm (:option :value (test-dataset1 i)
				       (format t "Dataset ~A" i)))))
	   (:input :type "submit" :value "Send"))))

(defpage foo "foo"
  (with-template "Foo"
    (:p "This is a test")))

(defparameter test-dataset 
  '(("Bob" . 81) ("Joe" . 82) ("Kathy" . 83)))

(defun json-dataset (dataset)
  (json:encode-json-alist-to-string
   `(("GraphName" . "Webdesign Data")
     ("GraphType" . "BarGraph")
     ("GraphTemplate" . 1)
     ("Xlabel" . "People")
     ("Ylabel" . "Scores")
     ("data" ,@dataset))))

	   
(defun test-dataset1 (i)
  (json-dataset 
   (mapcar (lambda (pair)
	     (cons (car pair) (ceiling (* 100 (cdr pair)))))
	   (let ((response (canonical-response-distribution *model* i)))
	     response))))
;;	     (subseq response 0 (min (length response) 4))))))

;; 