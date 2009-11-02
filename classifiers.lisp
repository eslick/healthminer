;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-

;;
;; Generic support for various kinds of classifiers
;;

(in-package :smart)

;;
;; CLASSIFIER INTERFACE
;;

(defgeneric train-classifier (classifier feature-set class &rest opts)
  (:documentation "Train the provided classifier with a single"))

(defgeneric predict-class (classifier feature-set &rest opts)
  (:documentation "Predict the class of the provided feature-vector"))

(defgeneric reset-classifier (classifier)
  (:documentation "Reset the stats of the classifier"))

;;
;; Managing feature spaces
;;

;; Classifiers operate on a bag of #'eq features, but often we want
;; to differentiate this bag into specific relations,locations, etc.
;; The easiest way to do this is to create various tuples that capture
;; the modifiers on top of the base feature (ie word+pos-tag+offset, etc)

(defclass feature-space ()
  ((map-id :reader feature-space-map-id :initform (hash :test #'eq))
   (map-vector :reader feature-space-map-vector :initform (hash :test #'equal))
   (dimensions :accessor feature-space-dimensions :initarg :dimensions)
   (index :reader feature-space-index :initform -1 :initarg :index)))

(defmethod get-feature-vector-id ((fspace feature-space) &rest atomic-features)
  (with-slots (map-id map-vector index dimensions) fspace
    (when (neq (length atomic-features) dimensions)
	(if (and (> dimensions 1) 
		 (eq (length atomic-features) 1) 
		 (listp (car atomic-features)) 
		 (eq (length (car atomic-features)) dimensions))
	    (setf atomic-features (car atomic-features))
	  (error "A single element must a list of atoms of size ~A" dimensions)))
    (aif-ret (gethash atomic-features map-vector)
	     (progn 
	       (setf (gethash (incf index) map-id) atomic-features)
	       (setf (gethash atomic-features map-vector) index)))))

(defmethod get-feature-vector ((fspace feature-space) id)
  (gethash id (feature-space-map-id fspace)))