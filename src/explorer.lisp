
(in-package :cl-user)

(defpackage :explorer
  (:use :cl :utils :hunchentoot :cl-who :json :weblocks))

(in-package :explorer)

;;
;; Setup Explorer webapp
;;

(weblocks:defwebapp 'explorer)

(defun start-explorer ()
  (setup-webdesign-dataset)
  (open-store :memory)
  (start-weblocks :debug t)

(defun stop-explorer ()
  (close-store *default-store*)
  (setf *default-store* nil)
  (stop-weblocks))

(defun reset ()
  (weblocks::reset-sessions))


;;
;; Entry points
;;

(defun init-user-session (comp)
  (with-flow comp
    (setf (widget-prefix-fn comp) #'render-header)
    (yield (make-main-page))))

(defun make-main-page ()
  (make-instance 'composite :widgets
		 (list
		  (make-instance 'flash :messages
				 (list "Welcome to the Collective Explorer - a technology preview"))
		  (make-navigation 'main-menu
;;				   'home (render-link nil "Home"  ??
				   'demonstration (make-demo-page)
				   'console (make-interaction-page)))))

(defun make-demo-page ()
  (let ((viewer (make-instance '
  (make-instance 'composite :widgets
		 (with-html-form :get (setf (
		 (list (:form 
			:method :post
			(render-dropdown 'demo-select
					 (list "query1" "query2" "query3" "query4")
					 :selected-value "query1")

(defun make-interaction-page ()
  (make-instance 'composite :widgets
		 (list (with-html
			 (:div (:p :style "font-style: italic;"
				   "Roses are red" (:br)))))))

;;
;; Explorer widgets
;;

(defwidget explorer-cli (widget)
  ((query :accessor ecli-query
	  :initform nil)
   (workspace :accessor ecli-workspace
	      :initarg :workspace)))

(defmethod render-widget-body ((obj explorer-cli) &rest args)
  (with-html-form (:post #'handle-query)
    (:label "Command: ")
    (:input :type "text" :value (ecli-query obj))))
	  

;;
;; Site format
;;

(defun render-header (&rest args)
  (declare (ignore args))
  (with-html
    (:div :class "header"
	  (with-extra-tags))))

(defmethod render-page-body :after (rendered-html)
  (declare (ignore rendered-html))
  (with-html
    (:div :class "footer"
	  (:p :id "system-info"
	      "Running on "
	      (str (concatenate 'string (server-type) " " (server-version)))
	      " (" (str (concatenate 'string (lisp-implementation-type) " "
				     (lisp-implementation-version))) ")")
	  (:p :id "contact-info"
	      "Ian Eslick, New Media Medicine  " (:a :href "mailto:eslick@media.mit.edu" "eslick@media.mit.edu") "."))))
