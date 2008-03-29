
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
    (:form :action "http://lamsight-dev.media.mit.edu/render/servlet/Renderer2" 
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
  '(("Bob's" . 81) ("Joe" . 82) ("Kathy" . 83)))

(defun json-dataset (dataset)
  (json:encode-json-alist-to-string
   `(("GraphName" . "Experience of Respondents")
     ("GraphType" . "BarGraph")
     ("GraphTemplate" . 1)
     ("Xlabel" . "Years")
     ("Ylabel" . "Number of people")
     ("Xsize" . "700")
     ("Ysize" . "400")
     ("chartbackgroundcolor" . "255,255,255")
     ("plotbackgroundcolor" . "200,200,200")
     ("seriesbackgroundcolor" . "70,50,200")
     ("includelegend"  . "false")
     ("labelfontcolor" . "0,0,0")
     ("titlefontsize" . 20)
     ("labelfontsize" . 12)
     ("titlefontcolor" . "0,0,0")
     ("data" ,@(clean-dataset dataset)))))

(defun clean-dataset (dataset)
  (loop for datapoint in dataset collect
       (if (stringp (car datapoint))
	   (cons (clean-string (car datapoint))
		 (cdr datapoint))
	   datapoint)))

(defun clean-string (label)
  (string-remove-characters label '(#\')))
	 

	   
(defun test-dataset1 (i)
  (json-dataset 
   (mapcar (lambda (pair)
	     (cons (car pair) (ceiling (* 100 (cdr pair)))))
	   (let ((response (canonical-response-distribution *model* i)))
	     response))))
;;	     (subseq response 0 (min (length response) 4))))))

;; 