(in-package :smart)

;; Word dictionary for use in visualizing text
;; - Create and add only
;; - No phrase aggregation

(defclass prefix-dictionary ()
  ((words :accessor words :initform (make-hash-table :test #'eq :size 100000))
   (sentences :accessor sentences :initform (make-hash-table :test #'eq :size 100000))))

;; Basic link access
(defun get-word-rec (dict word) 
  (aif-ret (gethash word (words dict))
    (setf (gethash word (words dict))
	  (list (make-array 10 :fill-pointer 0 :adjustable t)
		(make-array 10 :fill-pointer 0 :adjustable t)
		(make-array 100 :fill-pointer 0 :adjustable t)))))
(defun get-fwd-links (dict word) (first (get-word-rec dict word)))
(defun get-bck-links (dict word) (second (get-word-rec dict word)))
(defun get-sentence-links (dict word) (third (get-word-rec dict word)))

(defun has-link (links word) (find word links))
(defun push-word (word links) (vector-push-extend word links))
(defun add-word-link (links word)
  (unless (has-link links word)
    (push-word word links)))

;; Mutations
(defun add-word-pair (dict this next)
  (add-word-link (get-fwd-links dict this) next)
  (add-word-link (get-bck-links dict next) this))

(defparameter *sid* nil)

;; Load by sentence
(defun prefix-dict-index-message (message dict)
  (let ((sentences (message-sentences message 2)))
    (loop for sentence in sentences 
       for sid = (incf *sid*)
       do (setf (gethash sid (sentences dict)) sentence)
       do (loop for word in (phrase-words sentence)
	     do (push-word sid (get-sentence-links dict word))))))

;; (progn (setf 
;; 	(phrase-document 
;; 	 (loop with this = (aref vector 0)
;; 	    for i from 1 below (length vector)
;; 	    for next = (aref vector i)
;; 	    do (progn (add-word-pair dict this next)
;; 		      (setf this next))))))

(defun prefix-dict-index-messages (messages)
  (let ((dict (make-instance 'prefix-dictionary))
	(*sid* 0))
    (declare (special *sid*))
    (mapcar (curry2 'prefix-dict-index-message dict) messages)
    dict))

;; =====================================================================================
;; Compute prefix tree from word or words
;; =====================================================================================

(defun encode-prefix-tree (records)
  (json:encode-json-to-string records))

(defparameter *id* 0)

(defun compute-prefix-tree (dict words)
  (unless (valid-prefix-path dict words) 
    (return-from compute-prefix-tree nil))
  (let ((nodes (make-hash-table :test #'eq))
	(root (last1 words))
	(*id* -1))
    (declare (special *id*))
    (get-prefix-node nodes root)
    (compute-prefix-tree-aux root nodes dict 1)
    nodes))











#|

(defun valid-prefix-path (dict words)
  (loop with this = (first words)
       for next in (rest words)
       unless (prog1 (has-link (get-fwd-links dict this) next)
		(setf this next))
       do (return-from valid-prefix-path nil))
  t)

(defparameter *id* 0)
(defparameter *max-prefix-depth* 3)
	     
(defun compute-prefix-tree (dict words)
  (unless (valid-prefix-path dict words) 
    (return-from compute-prefix-tree nil))
  (let ((nodes (make-hash-table :test #'eq))
	(root (last1 words))
	(*id* -1))
    (declare (special *id*))
    (get-prefix-node nodes root)
    (compute-prefix-tree-aux root nodes dict 1)
    nodes))

(defun get-terminal-ids ()
  (aif-ret *terminal-ids*
    (setf *terminal-ids* 
	  (mapcar #'id-for-token '("." "!" "?")))))

(defun compute-prefix-tree-aux (root nodes dict depth)
  "Add nodes and links for each target word"
  (let ((links (get-fwd-links dict root)))
    (assert links)
    (loop for target across (get-fwd-links dict root)
       unless (member target (get-terminal-ids))
       do (progn (add-prefix-link nodes root target)
		 (format t "~A->~A~%" root target)
		 (unless (>= depth *max-prefix-depth*)
		   (compute-prefix-tree-aux target nodes dict (1+ depth)))))))

(defun add-prefix-link (nodes word target)
  (let ((tid (assoc-get 'id (get-prefix-node nodes target))))
    (push tid (cdr (assoc 'children (get-prefix-node nodes word))))))

(defun get-prefix-node (nodes word)
  (aif-ret (gethash word nodes)
    (setf (gethash word nodes)
	  `((id . ,(incf *id*))
	    (name . ,(token-for-id word))
	    (type . words)
	    (children . nil)))))

|#
  


