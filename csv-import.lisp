;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: lamsight -*-

(in-package :smart)


(eval-when (:compile-toplevel)
  (proclaim '(optimize (speed 2) (safety 1) (space 1))))

;; We are going to build a database from a raw survey dataset.  
;; Database is in CSV format. quotes escape commas.
;; The first line is the set of questions
;; The second and subsequent lines are the set of answers, aligned to the header questions


;; Extract CSV into usable list form

(defun extract-csv-dataset (file &optional count)
  "Extract the first line as the header,
   the rest as a list of expanded fields
   the body is a list of a list of strings"
  (with-open-file (str file)
    (let ((header (csv->list (read-line str)))
	  (body-list (extract-csv-lines str nil count)))
      (values header body-list))))

(defun extract-csv-lines (stream list &optional count)
  "Simple recursive application of csv->list"
  (let ((line (read-line stream nil 'end)))
    (if (or (eq line 'end)
	    (and count (>= 0 count)))
	(nreverse list)
	(extract-csv-lines stream (cons (csv->list line) list)
			   (when count (1- count))))))
		
(defun csv->list (line)
  "Turn CSV line into a list of string fields.  
   We need to ignore commas between quotations and remove quotations"
  (field-line->list line #\, '(#\")))

(defun field-line->list (line field-separator escape-chars &optional (drop-escapes t))
  "Extracts a set of field delimited by field-separator, but ignoring those separators
   if they lie between balanced instances of escape-chars.  This does not handle 
   single character escapes"
  (let ((list nil)
	(escape-mode nil)
	(start 0))
  (flet ((escape-p (c)
	   (member c escape-chars))
	 (toggle-escape ()
	   (if escape-mode 
	       (setq escape-mode nil)
	       (setq escape-mode t))))
    (loop for char across line 
	  for offset from 0 do
	 (cond ((escape-p char)
		(toggle-escape))
	       ((and (eq char field-separator) 
		     (not escape-mode))
		(if (= start offset) 
		    (progn 
		      (incf start)
		      (push nil list))
		    (progn 
		      (let ((entry (subseq line start offset)))
			(when drop-escapes
			  (delete-if #'escape-p entry))
			(push entry list))
		      (setq start (1+ offset)))))))
    (nreverse list))))

(defmethod extract-multiple-value-line (line)
  (field-line->list line #\, '(#\( #\) #\")))

(defparameter *not-multi-valued* '(11))

(defmethod multiple-value-data-p (line offset)
  (if (member offset *not-multi-valued*)
      nil
      (> (length (extract-multiple-value-line line)) 1)))

;;
;; Build a model of the data and reformat into an efficient queryable format
;;

(defclass survey-data ()
  ((questions :accessor questions :initarg :questions)
   (responses :accessor responses :initarg :responses)
   (source-file :accessor source-file :initarg :file)
   (record-count :accessor record-count :initarg :record-count :initform nil)))

(defmethod initialize-instance :after ((sd survey-data) &rest args)
  (when (slot-boundp sd 'source-file)
    (multiple-value-bind (header body)
	(extract-csv-dataset (source-file sd) (awhen (record-count sd) it))
      (setf (questions sd) header)
      (setf (responses sd) body))))
	  
(defmethod all-field-values ((sd survey-data) field-no)
  (mapcar #'(lambda (response)
	      (nth field-no response))
	  (responses sd)))

(defmethod all-unique-field-values ((sd survey-data) field-no)
  (remove-nulls
   (remove-duplicates 
    (all-field-values sd field-no)
    :test #'equal)))

(defmethod all-unique-multiple-values (value-list)
  (remove-duplicates
   (mapcan #'extract-multiple-value-line value-list)
   :test #'equal))

;;
;; Survey database model; computes metadata on field values, etc
;;

(defparameter *open-field-value-threshold* 100)

(defclass survey-field ()
  ((id :accessor field-id :initarg :id)
   (model :accessor field-model :initarg :model)
   (type :accessor field-type :initarg :type)
   (open-p :accessor field-open-p :initarg :open-p)
   (values :accessor field-values :initarg :values))
  (:documentation "survey-field provides an abstraction for the type of a field,
                   for now simply single-value and multi-value as well as the
                   range of possible values.  predicate open-p if open-ended answers"))

(defmethod field-question ((field survey-field))
  (nth (field-id field) (questions (field-model field))))

(defun make-field (offset survey-model)
  (let ((field (make-instance 'survey-field :id offset :model survey-model))
	(all-values (all-unique-field-values survey-model offset))
	open-p type)
    (cond ((some #'(lambda (values) (multiple-value-data-p values offset)) all-values)
	   (setq type :multi)
	   (setf all-values (all-unique-multiple-values all-values)))
	  ((> (length all-values) *open-field-value-threshold*)
	   (setq open-p t))
	  (t (setq type :single)))
    (setf (field-open-p field) open-p)
    (setf (field-type field) type)
    (if open-p
	(setf (field-values field) nil)
	(setf (field-values field) all-values))
    field))

(defclass survey-model (survey-data)
  ((fields :accessor fields :initarg :fields)))

(defmethod initialize-instance :after ((sm survey-model) &rest args)
  (declare (ignore args))
  (setf (fields sm) (make-fields sm))
  #+allegro (excl::gc t)
  (loop for field in (fields sm) do
       (when (eq (field-type field) :multi)
	 (loop for response in (responses sm) do
	      (setf (nth (field-id field) response)
		    (extract-multiple-value-line 
		     (nth (field-id field) response)))))))

(defmethod make-fields ((sm survey-data))
  (setf (fields sm)
	(loop for offset from 0 upto (1- (length (questions sm)))
	   collecting (make-field offset sm))))

;;
;; Methods to query the data
;;

(defun survey-questions (survey-model)
  (loop 
     for q in (questions survey-model) 
     for i from 0 collect (list i q)))

(defun print-survey-questions (survey-model)
  (mapc #'(lambda (pair) 
	    (format t "~A: ~A~%" (first pair) (second pair)))
	(survey-questions survey-model)))


;; Zuoyu: this is a quick hack to illustrate some of the details; should be 
;; cleaned up to support the query types and graph types we want to enable.  
;; Get json data format input from John...


(defun json-percent-distribution (sm field-id)
  (json:encode-json-alist-to-string
   (canonical-response-distribution sm field-id)))

(defun canonical-response-distribution (sm field-id)
;;  (sorted-alist
   (normalize-alist-series
    (response-distribution sm field-id)))

(defun get-field (sm field-id)
  (nth field-id (fields sm)))

(defun get-field-question (sm field-id)
  (field-question (nth field-id (fields sm))))

(defun response-distribution (sm field-id)
  "Returns an alist of value occurances to a given question"
  (let ((field (nth field-id (fields sm))))
;;    (format t "Computing the distribution of responses to: ~A~%With possible values:"
;;	    (field-question field))
    (if (eq (field-type field) :single)
	(single-response-distribution sm field)
	(multi-response-distribution sm field))))

(defun single-response-distribution (sm field)
    ;; Init entries
  (let ((hash (make-hash-table :test #'equal))
	(total 0)
	(responses 0))
    (mapc (lambda (value)
;;	    (print value)
	    (setf (gethash value hash) 0))
	  (field-values field))
    ;; Count responses
    (loop for response in (responses sm) do
	 (let ((value (nth (field-id field) response)))
	   (incf total)
	   (when value
	     (incf responses)
	     (incf (gethash value hash)))))
    (values (hash-items hash) (/ responses total))))

(defun multi-response-distribution (sm field)
  (let ((total 0)
	(responses 0)
	(hash (make-hash-table :test #'equal)))
    (mapc (lambda (value)
	    (setf (gethash value hash) 0))
	  (field-values field))
    (loop for response in (responses sm) do
	 (let ((values (nth (field-id field) response)))
	   (incf total)
	   (when values
	     (incf responses)
	     (loop for value in values do
		  (incf (gethash value hash))))))
    (values (hash-items hash) (/ responses total))))
	 

;; Some alist as series helpers

(defun sorted-alist (alist &optional (pred #'<))
  (sort alist pred :key #'cdr))

(defun normalize-alist-series (alist)
  "take an alist (x-label . value) and normalize values"
  (let ((total (apply #'+ (cdrs alist))))
    (mapcar #'(lambda (pair)
		(cons (car pair) (coerce (/ (cdr pair) total) 'float)))
	    alist)))

;; Example run

;; Build the model
; (setf model (make-instance 'survey-model :file "filename"))

;; See survey questions
; (survey-questions model)

;; Pick a question
; (canonical-response-distribution model 8)

;; In serialized format for Java!
; (json-percent-distribution model 8)
		   