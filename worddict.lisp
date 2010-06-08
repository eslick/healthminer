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
(defun get-sentence-links (dict word) (third (get-word-rec dict (id-for-token word))))

(defun has-link (links word) (find word links))
(defun push-word (word links) 
  (vector-push-extend word 1000))
(defun push-location (obj offset links)
(defun add-word-link (links word)
  (unless (has-link links word)
    (push-word word nil links)))

;; Record access
(defun get-dict-sentence (dict sid)
  (gethash sid (sentences dict)))

;; Mutations
(defun add-word-pair (dict this next)
  (add-word-link (get-fwd-links dict this) next)
  (add-word-link (get-bck-links dict next) this))

(defparameter *sid* nil)
(defvar *prefix-dict* nil)

(defun learn-prefix-dict (count)
  (setf *prefix-dict*
	(prefix-dict-index-messages (subseq *all-messages* 0 count))))

;; Learn a set of messages
(defun prefix-dict-index-messages (messages)
  (let ((dict (make-instance 'prefix-dictionary))
	(*sid* 0))
    (declare (special *sid*))
    (mapcar (curry2 'prefix-dict-index-message dict) messages)
    dict))

;; Capture sentences for each message
(defun prefix-dict-index-message (message dict)
  (let ((sentences (message-sentences message 2)))
    (loop for sentence in sentences 
       for sid = (incf *sid*)
       do (setf (gethash sid (sentences dict)) sentence) ;; add sent
       do (prefix-dict-index-bigrams dict sid (phrase-words sentence)))
    (incf *sid*)))

(defun prefix-dict-index-bigrams (dict sid words)
  (loop ;; with prior = nil
     for word in words
     for offset from 0
     do (progn 
	  (push-word sid offset (get-sentence-links dict word)))))
;;	  (when prior (add-word-pair dict prior word))
;;	  (setf prior word))))
       
;; =====================================================================================
;; Compute prefix tree from word or words
;; =====================================================================================

(defun prefix-right-tree (dict term)
  "Return a prefix tree that expands left-to-right"
  (prefix-right-subtree dict term (prefix-word-context dict term)))

(defun prefix-word-context (dict word)
  "Return a list of context entries for the sentences containing word, to get
   the root context for determining a subtree"
  (mapcar (lambda (location)
	    (dbind (sid . offset) location
	      (list sid offset (get-dict-sentence dict sid))))
	  (array->list (get-sentence-links dict word))))

(defun prefix-right-subtree (dict term context)
  "Where context is a prefix + a list of (sid offset sentence) triples"
  (if (or (= (length context) 1)
	  (identical-context-p context 5))
      (list (concat-words 
	     (mapcar #'token-for-id 
		     (subseq (phrase-words (third (first context)))
			     (second (first context))))) nil)
      (let ((next-context (next-word-contexts context)))
	(if (= (length next-context) 1)
	    (let ((new-tree (prefix-right-subtree dict (caaar next-context)
						  (cdrs (car next-context)))))
	      (setf (first new-tree)
		    (strcat (if (numberp term) (token-for-id term) term) " " (first new-tree)))
	      new-tree)
	     (list (if (numberp term) (token-for-id term) term)
		(mapcar (f (sc) (prefix-right-subtree dict (caar sc) (cdrs sc)))
			next-context))))))

;; identify substrings

(defun next-word-contexts (context)
  "Get the unique subsets of words as determined by the next word in each context sentence"
  (ndistinct (remove-nulls (mapcar 'next-word-context context)) :key #'car))

(defun next-word-context (context)
  "Return the next word in the sentence prepended to the context elements"
  (dbind (sid offset sentence) context
    (awhen (phrase-next-token offset sentence)
      (list it sid (1+ offset) sentence))))

(defun phrase-next-token (offset phrase)
  (let ((new (1+ offset)))
    (if (>= new (phrase-length phrase))
	nil
	(get-token-id phrase new))))

(defun identical-context-p (context limit)
  (labels ((next-tokens (offset)
	     (mapcar (lambda (elt) (phrase-next-token (+ (second elt) offset) (third elt)))
		     context)))
    (> (loop
	  for i from 0
	  for tokens = (next-tokens i)
	  unless (and (< i 7)
		      (or (some #'null tokens)
			  (every (curry #'equal (first tokens)) (rest tokens))))
	  do (return i))
       limit)))

	    
;; =======================

(defun serialize-tree (tree)
  (let ((hash (make-hash-table :test #'eq))
	(id 0))
    (labels ((rec (subtree)
	       (let ((my-id (1- (incf id))))
		 (dbind (term children) subtree
		   (setf (gethash my-id hash)
			 `(("id" . ,my-id)
			   ("name" . ,term)
			   ("type" . "words")
			   ("children" . ,(mapcar #'rec children)))))
		 my-id)))
      (rec tree)
      (hash-values hash))))

(define-api-handler "/wordlattice/right" (method params json)
  (serialize-tree (prefix-right-tree *prefix-dict* (assoc-get :query params))))


;; =======================


;; (defun encode-prefix-tree (records)
;;   (json:encode-json-to-string records))

;; (defparameter *id* 0)

;; (defun compute-prefix-tree (dict words)
;;   (unless (valid-prefix-path dict words) 
;;     (return-from compute-prefix-tree nil))
;;   (let ((nodes (make-hash-table :test #'eq))
;; 	(root (last1 words))
;; 	(*id* -1))
;;     (declare (special *id*))
;;     (get-prefix-node nodes root)
;;     (compute-prefix-tree-aux root nodes dict 1)
;;     nodes))

;; (defun valid-prefix-path (dict words)
;;   (loop with this = (first words)
;;        for next in (rest words)
;;        unless (prog1 (has-link (get-fwd-links dict this) next)
;; 		(setf this next))
;;        do (return-from valid-prefix-path nil))
;;   t)

;; (defparameter *id* 0)
;; (defparameter *max-prefix-depth* 3)
;; (defparameter *terminal-ids* nil)
	     
;; ;; (defun compute-prefix-tree (dict words)
;; ;;   (unless (valid-prefix-path dict words) 
;; ;;     (return-from compute-prefix-tree nil))
;; ;;   (let ((nodes (make-hash-table :test #'eq))
;; ;; 	(root (last1 words))
;; ;; 	(*id* -1))
;; ;;     (declare (special *id*))
;; ;;     (get-prefix-node nodes root)
;; ;;     (compute-prefix-tree-aux root nodes dict 1)
;; ;;     nodes))

;; (defun get-terminal-ids ()
;;   (aif-ret *terminal-ids*
;;     (setf *terminal-ids* 
;; 	  (mapcar #'id-for-token '("." "!" "?")))))

;; (defun compute-prefix-tree-aux (root nodes dict depth)
;;   "Add nodes and links for each target word"
;;   (let ((links (get-fwd-links dict root)))
;;     (assert links)
;;     (loop for target across (get-fwd-links dict root)
;;        unless (member target (get-terminal-ids))
;;        do (progn (add-prefix-link nodes root target)
;; 		 (format t "~A->~A~%" root target)
;; 		 (unless (>= depth *max-prefix-depth*)
;; 		   (compute-prefix-tree-aux target nodes dict (1+ depth)))))))

;; (defun add-prefix-link (nodes word target)
;;   (let ((tid (assoc-get 'id (get-prefix-node nodes target))))
;;     (push tid (cdr (assoc 'children (get-prefix-node nodes word))))))

;; (defun get-prefix-node (nodes word)
;;   (aif-ret (gethash word nodes)
;;     (setf (gethash word nodes)
;; 	  `((id . ,(incf *id*))
;; 	    (name . ,(token-for-id word))
;; 	    (type . words)
;; 	    (children . nil)))))

  


