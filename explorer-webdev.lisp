;;
;; Functions to analyze the webdev database
;;

(in-package :explorer)

(defvar *model* nil)

(defun setup-webdesign-dataset ()
  (unless *model*
    (setf *model* (make-instance 'smart::survey-model 
				 :file "/Users/eslick/Work/newmed/smart/websurvey.csv"))))


(defun webdev-test-dataset (i)
  "Generate a distribution for responses i"
  (json-dataset 
   (mapcar (lambda (pair)
	     (cons (car pair) (ceiling (* 100 (cdr pair)))))
	   (let ((response (smart::canonical-response-distribution *model* i)))
	     response))))

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

