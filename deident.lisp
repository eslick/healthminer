(in-package :smart)

;;(defun print-deidentified-message (message)
;;  "Returns subject, body only for now"
;;  (
;;  (deidentify-tokens (message-words message)))

;; ==============================================
;; Fast linear scanning w/o destroying structure
;; ==============================================

(defun de-identify-string-clean (string)
  (let* ((str (copy-seq string))
	 (de  (de-identify-string string))
	 (str-end (length str))
	 (de-end (length de)))
    (labels ((rec (i j)
	       (unless (or (eq i str-end)
			   (eq j de-end))
		 (let ((ic (char str i))
		       (jc (char de j)))
;;		 (print (cons ic jc))
		 (cond ((equal ic jc)
			(rec (1+ i) (1+ j)))
		       ((is-char-whitespace jc)
			(rec i (1+ j)))
		       ((or (is-char-whitespace ic)
			    (member ic '(#\Newline #\Return)))
			(rec (1+ i) j))
		       ((eq jc #\-)
			(setf (char str i) jc)
			(rec (1+ i) (1+ j)))
		       (t (rec (1+ i) (1+ j))))))))
;;		       (t (error "Unknown case:~%'~A'~%'~A'"
;				 (subseq str i (min (+ i 20)
;;						    (length str)))
;				 (subseq de j (min (+ j 20)
;;						   (length de))))))))))
      (rec 0 0))
    str))

(defun de-identify-string (string)
  (concat-words 
   (tokens-for-ids
    (de-identify-doc (vector-tag string)))))
;;  (concat-words (tokens-for-ids (de-identify-tokens (array->list (string->token-array string))))))

(defun de-identify-doc (doc)
  (loop for id across (document-text doc)
        for tag across (document-tags doc)
       collect (if (ident-token-p id tag)
		   (make-deident-id id)
		   id)))


(defun de-identify-tokens (ids)
  (loop 
     for id in ids
     collect (if (ident-token-p id)
		 (make-deident-id id)
		 id)))

(defun make-deident-id (id)
  (let ((len (length (token-for-id id))))
    (id-for-token (make-string len :initial-element #\-))))

;; =======================================
;; Contextual identifiers
;; =======================================

;; =======================================
;; Token Identifiers
;; =======================================

(defun ident-token-p (id tag)
  (let ((string (token-for-id id)))
    (or (person-name-p string tag)
	(email-address-p string))))

(defun person-name-p (string tag)
  (if (and (upper-case-p (char string 0)) (noun-pos-p tag))
      (or (lookup-first-name string)
	  (lookup-name string))))

(defun email-address-p (string)
  (or (ppcre:scan ".*@.*\\.(com|edu|org|it|de|uk|me|us|net)" (string-downcase string))))

;; ========================================
;; Name Database
;; ========================================

(defparameter *name-dir* "/Users/eslick/Desktop/")
(defparameter *name-files* '("dist-last.txt" "dist-female.txt" "dist-male.txt"))

(defvar *first-name-map* nil)
(defvar *last-name-map* nil)

(defun lookup-name (name)
  (unless (and *first-name-map* *last-name-map*)
    (init-name-hash))
  (or (gethash (string-upcase name) *first-name-map*)
      (gethash (string-upcase name) *last-name-map*)))

(defun lookup-first-name (name)
  (unless *first-name-map*
    (init-name-hash))
  (gethash (string-upcase name) *first-name-map*))

(defun init-name-hash ()
  (setf *first-name-map* (make-hash-table :test #'equal))
  (setf *last-name-map* (make-hash-table :test #'equal))
  (labels ((load-file (hash file)
	     (let ((path (make-pathname :directory *name-dir* :defaults file)))
	       (with-open-file (stream path)
		 (awhile (read-line stream nil nil)
		   (setf (gethash (first (extract-words it)) hash) t))))))
    (load-file *last-name-map* (first *name-files*))
    (mapc (curry #'load-file *first-name-map*) (rest *name-files*)))
  t)
		      
     
       