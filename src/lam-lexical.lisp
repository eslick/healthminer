(in-package :smart-lex)

(defun lexpat-match (pattern doc &optional (start 0))
  (let ((pat (convert-pattern pattern)))
    (match-aux pat (new-state pat doc start))))

(defun lexpat-matches (pattern doc &optional (start 0))
  (mvbind (matches mstart mend) 
      (lexpat-match pattern doc start)
    (when matches
      (cons matches (lexpat-matches pattern doc mend)))))
      

;; Patterns

(defun convert-pattern (pattern)
  (loop for elt in pattern collect
       (cond ((stringp elt)
	      (id-for-token elt))
	     (t elt))))

(defun var-p (var) (symbolp var))
(defun wildcard-p (var) (and (var-p var) (equal "?" (symbol-name var))))
(defun token-p (var) (numberp var))

;; Maintain state

(defstruct state orig-pattern doc start last offset matches current-var)


;; State sugar

(defmacro with-lexpat-state ((state) &body body)
  `(with-struct-slots (state orig-pattern doc start last offset matches current-var) ,state
     (declare (ignorable orig-pattern doc start last offset matches current-var))
     ,@body))

(defmacro defun-lexpat (name args &body body)
  `(defun ,name ,args
     (with-lexpat-state (,(last1 args))
       ,@body)))

(defun new-state (pattern doc offset)
  (make-state :orig-pattern pattern :doc doc :start offset :last offset :offset offset))

(defun-lexpat reset-state (state)
  (let ((base (max (1+ start) last)))
    (setf offset base
	  start base
	  last base
	  matches nil
	  current-var nil)
    state))


;; Matcher

(defparameter *max-window-size* 15)

(defun match-aux (pattern state)
  (assert (listp pattern))
  (when (terminate-p pattern state)
    (return-from match-aux (finish pattern state)))
  (with-lexpat-state (state)
    (let ((var (first pattern))
	  (token (aref (document-text doc) offset)))
      (cond ;;((wildcard-p var)
	    ;; (increment pattern state))
	    ((var-p var) ;; if a variable
	     (consume-variable pattern state))
	    ((match-overflow-p state) ;; overrun variable window
	     (finish pattern state))
	    ((token-p var) ;; if a token
	     (cond ((= var token)
		    (consume-token pattern state))
		   (current-var
		    (increment pattern state))
		   (t (finish pattern state))))
	    ;;	    ((wildcard-p var)
	    ;;	     (if (wildcard-exceeded state)
	    ;;		 (finish pattern state)
	    ;;		 (increment pattern state)))
	    (t (increment pattern state))))))

(defun-lexpat terminate-p (pattern state)
  (or (null pattern) (>= offset (length (document-text doc)))))

(defun-lexpat match-overflow-p (state)
  (> (- offset last) *max-window-size*))

(defun-lexpat increment (pattern state)
  (incf offset)
  (match-aux pattern state))

(defun-lexpat consume-token (pattern state)
  (when current-var (record-variable state))
  (setf last offset)
  (incf offset)
  (match-aux (rest pattern) state))

(defun-lexpat record-variable (state)
  (push (cons current-var 
	      (make-phrase-from-vdoc doc (if (= last start) last (1+ last))
				     (- (min offset (- (length (document-text doc)) 10))
					(1+ last))))
	matches)
  (setf current-var nil))

(defun-lexpat consume-variable (pattern state)
  (assert (not current-var))
  (setf current-var (first pattern))
  (match-aux (rest pattern) state))

(defun-lexpat finish (pattern state)
  (cond ((null pattern)
	 (when current-var
	   (setf offset (min (+ offset *max-window-size*) (1- (length (document-text doc)))))
	   (record-variable state))
	 (results state))
	((terminate-p pattern state) nil)
	(t (match-aux orig-pattern (reset-state state)))))

(defun-lexpat results (state)
  (values (nreverse matches) start offset))

(defun capture (array start end)
  (array->list (subseq array start end)))

;;(defun capture-left (array offset size)
;;  (array->list (subseq array offset (max (- offset size) 0))))

;;(defun capture-right (array offset size)
;;  (array->list (subseq array offset (min (length array) (+ offset size)))))
