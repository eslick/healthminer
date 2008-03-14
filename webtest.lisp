
(in-package :smart)

(defparameter *users* '(("eslick" "food" (:scary-hair :evil))))

(defvar *our-mutex* (hunchentoot-mp:make-lock "our-lock"))
(defvar *events* '())
(defvar *model* nil)
  
(defun launch-server ()
  (hunchentoot:start-server :port 4242)
  (setf *model* (make-instance 'survey-model 
			       :file "/Users/eslick/Work/newmed/smart/websurvey.csv")))

(defun add-event (user text)
  (hunchentoot-mp:with-lock (*our-mutex*)
    (push `(,user ,text) *events*)))

(defmacro with-template (title &body body)
  `(with-html-output-to-string (*standard-output*)
     (:html
      (:head (:title (fmt "Graph demo - ~a" ,title)))
      (:body (:h1 "Graph demo")
	     (:div (:a :href "/" "Main") " - "
		   (:a :href "/events" "Events"))
	     ,@body))))

(defmacro defpage (name url &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (push (create-regex-dispatcher ,(format nil "^/~a$" url) ',name)
	   *dispatch-table*)))



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

	   
(defun test-dataset1 (i)
  (json-dataset 
   (mapcar (lambda (pair)
	     (cons (car pair) (ceiling (* 100 (cdr pair)))))
	   (let ((response (canonical-response-distribution *model* i)))
	     (subseq response 0 (min (length response) 4))))))