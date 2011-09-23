;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: eventminer -*-

(in-package :smart)

;; naive bayes classification for C categories

;; A classifier has a set of Categories 'C' which are trained against
;; a corpus of evidence 'E' where each piece of evidence consists of
;; a category C_n and a set of features f_n.  We can add more structure
;; to this later, such as ordering, tenses, etc.  For now we stick to
;; a bag of POS tags and words.

;; A training set results in a trained classifier having:
;; - Total samples (universe U)
;; - probability of a word or tag given all samples
;; - conditional probability of ...

(defun classify (text)
  (classification (score (extract-features text))))

;;;; ----------------------------
;;;; Data structures
;;;; ----------------------------

(defclass word-feature ()
  ((word       
    :initarg :word
    :accessor word
    :initform (error "Must supply :word")
    :documentation "The word this feature represents.")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "Number of spams we have seen this feature in.")
   (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "Number of hams we have seen this feature in.")))

(defvar *feature-database* (make-hash-table :test #'equal))

(defvar *total-spams* 0)
(defvar *total-hams* 0)

(defun clear-database ()
  (setf
   *feature-database* (make-hash-table :test #'equal)
   *total-spams* 0
   *total-hams* 0))

(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))

(defun extract-bayes-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{1,}" text)
   :test #'string=))

(defun extract-features (text)
  (mapcar #'intern-feature (extract-bayes-words text)))

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))

;;;; --------------------------------
;;;; Training weights over words
;;;; --------------------------------

(defun train (text type)
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))

(defun increment-count (feature type)
  (ecase type
    (ham (incf (ham-count feature)))
    (spam (incf (spam-count feature)))))

(defun increment-total-count (type)
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))

(defun spam-probability (word)
  (with-slots (spam-count ham-count) word
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
          (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))

(defun bayesian-spam-probability (word assumed-probability weight)
  (let ((basic-probability (spam-probability word))
        (data-points (+ (spam-count word) (ham-count word))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

;;;; -------------------------------
;;;; classifying and scoring material
;;;; --------------------------------

(defparameter *max-ham-score* 0.4)
(defparameter *min-spam-score* 0.6)

(defun classification (score)
  (values
   (cond
     ((<= score *max-ham-score*) 'ham)
     ((>= score *min-spam-score*) 'spam)
     (t 'unsure))
   score))

(defun score (features)
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature 1/2 1) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (word)
  (with-slots (spam-count ham-count) word
    (and (zerop spam-count) (zerop ham-count))))

;;;; ---------------------------------------------------
;;;; FISHER Algorithm for determining the probability
;;;;   a distribution diverges from random

(defun fisher (probs number-of-probs)
  "The Fisher computation described by Robinson."
  (inverse-chi-square 
   (* -2 (log-of-product probs))
   (* 2 number-of-probs)))

;; Ensure no underflow when multiplying probabilities

(defun decoded (float &optional (exponent 0))
  (multiple-value-bind (signif exp sign) (decode-float float)
    (values (* signif sign) (+ exp exponent))))

(defun decoded-product (significand exponent float)
  (multiple-value-bind (s e) (decoded float exponent)
    (decoded (* significand s) e)))

(defun log-of-decoded (significand exponent)
  (+ (log significand) (* exponent (log (float-radix significand)))))

(defun log-of-product (numbers)
  (loop with significand = 1.0
        with exponent    = 0
        for number in numbers do
        (multiple-value-setq (significand exponent)
          (decoded-product significand exponent number))
        finally (return (log-of-decoded significand exponent))))

(defun inverse-chi-square (value degrees-of-freedom)
  "Probability that chi-square >= value with given degrees-of-freedom.
   Based on Gary Robinson's Python implementation."
  (assert (evenp degrees-of-freedom))
  ;; Due to rounding errors in the multiplication and exponentiation
  ;; the sum computed in the loop may end up a shade above 1.0 which
  ;; we can't have since it's supposed to represent a probability.
  (min 
   (loop with m = (/ value 2)
      for i below (/ degrees-of-freedom 2)
      for prob = (exp (- m)) then (* prob (/ m i))
      summing prob)
   1.0))