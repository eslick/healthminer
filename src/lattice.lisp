(in-package :smart)

;;
;; Sentence
;;

(defclass sentence (phrase)
  ((author :accessor author :initarg :author)
   (messsgae :accessor message :initarg :message)))

(defmethod as-sentence ((p phrase))
  (change-class p 'sentence))

(defun has-next-token-p (sentence offset)
  (< (1+ offset) (phrase-length sentence)))

(defun has-prev-token-p (sentence offset)
  (declare (ignore sentence))
  (> offset 0))


;;
;; Index
;;

(defvar *dictionaries* (make-hash-table :test #'equalp))

(defun add-dict (name dict)
  (setf (gethash name *dictionaries*) dict))

(defun lookup-dict (name)
  (gethash name *dictionaries*))

;; ---------------------------

(defclass sentence-dictionary ()
  ((words :accessor words :initform (make-hash-table :test #'eq :size 100000))
   (sentences :accessor sentences :initform (make-hash-table :test #'eq :size 100000))
   (last-sid :accessor last-sid :initform 0)))

(defmethod add-sentence ((dict sentence-dictionary) sentence)
  "Index anything that stores sequences of tokens and implements (phrase-words)"
  (declare (optimize (speed 3) (safety 1)))
  (let ((sid (incf (last-sid dict))))
    (setf (gethash sid (sentences dict)) sentence)
    (loop 
       with text = (the (vector fixnum *) (document-text (phrase-document sentence)))
       for i fixnum from (phrase-start sentence) to (phrase-end sentence)
       for offset fixnum from 0
       do (add-word-reference offset (aref text i) sid dict))
;;    (loop for word in (phrase-words sentence)
;;       for offset from 0
;;       do (add-word-reference offset word sid dict))
    sid))

(defmethod add-message ((dict sentence-dictionary) message)
  (let ((sentence-phrases (message-sentences message 2))
	(author (sender message)))
    (labels ((add-author (sentence)
	       (setf (author sentence) 
		     (typecase author
		       (string author)
		       (null nil)
		       (t (user-name author))))
	       sentence)
	     (insert-message (message sentence)
	       (setf (message sentence) message)
	       sentence))
      (mapc (compose (curry 'add-sentence dict) 
		     (curry #'insert-message message) 
		     #'add-author 'as-sentence)
	    sentence-phrases))
    t))

(defun add-messages (dict &optional count (messages *all-messages*))
  (when count (setf messages (subseq messages 0 count)))
  (mapc (curry 'add-message dict) messages)
  dict)

(defun load-lattice-datasets ()
  (unless *all-messages* (init-lam-listserv-db))
  (add-dict "LAM" (add-messages (make-instance 'sentence-dictionary)))
  (unless *acor-db* (open-acor-db))
  (add-dict "cancer" (add-messages (make-instance 'sentence-dictionary)
					nil (with-store (*acor-db*)
					  (get-instances-by-class 'email)))))

(defmethod get-sentence ((dict sentence-dictionary) sid)
  (gethash sid (sentences dict)))

(defun add-word-reference (offset word sid dict)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum sid word offset))
  (let ((vector (gethash word (words dict))))
    (declare (type vector vector))
    (unless vector
      (setf vector (the vector (add-word-reference-vector word dict))))
    (vector-push-extend sid vector 1000)
    (vector-push-extend offset vector 1000)))

(defun add-word-reference-vector (word dict)
  (setf (gethash word (words dict))
	(make-array 20 :element-type 'fixnum :fill-pointer 0 :adjustable t)))

(defmethod word-locations ((dict sentence-dictionary) word)
  "Return a set of sentence references for a word in a sentence dict"
  (unless (numberp word) (setq word (id-for-token word)))
  (gethash word (words dict)))

(defun array->sentence-references (array)
  "Given a array encoding #(oid offset oid offset) return pairs of (oid . offset)"
  (assert (array-has-fill-pointer-p array))
  (loop for i from 0 below (fill-pointer array) by 2 
       collect (cons (aref array i) (aref array (1+ i)))))

(defparameter *max-locations* nil)

(defun map-locations (fn locations)
  (declare (optimize (speed 3) (safety 1))
	   (type (or null (array fixnum *)) locations))
  (when locations
    (assert (array-has-fill-pointer-p locations))
    (loop for i fixnum from 0 below (if *max-locations*
					(the fixnum (min (fill-pointer locations) (* 2 *max-locations*)))
					(fill-pointer locations)) by 2 
       do (funcall fn (aref locations i) (aref locations (1+ i))))))
 
;;
;; Lattice nodes
;;

(defclass lattice-node ()
  ((term :accessor term :initarg :term :initform nil)
   (type :accessor type :initarg :type :initform :token)
   (children :accessor children :initarg :children :initform nil)
   ;; Metadata
   (leaves :accessor leaf-count :initform 1)
   (sentence :accessor origin-sentence :initarg :sentence :initform nil)
   (offset :accessor origin-offset :initarg :offset :initform nil)))

(defun make-lattice-node (sentence offset &key make-child-p (type :token))
  (aprog1 (make-instance 'lattice-node
			 :sentence sentence
			 :offset offset
			 :term (get-token-id sentence offset)
			 :type type)
    (when make-child-p (extend-lattice-node it))))

(defmethod print-object ((node lattice-node) stream)
  (format stream "#<LATTICE '~A' ~A >"
	  (term-text node) ;;(type node) 
	  (aif (children node) 
	       (if (listp it)
		   (length it)
		   ":term")
	       ":unex")))

;; Lattice node helpful utilities

(defmethod node-equal ((n1 lattice-node) (n2 lattice-node))
  (= (get-lemma-for-id (term n1)) 
     (get-lemma-for-id (term n2))))

(defmethod node-eq ((n1 lattice-node) (n2 lattice-node))
  (and (eq (origin-sentence n1) (origin-sentence n2))
       (eq (origin-offset n1) (origin-offset n2))))

(defmethod terminal-node-p ((node lattice-node)) (eq (children node) :terminal))
(defmethod branching-node-p ((node lattice-node)) (and (listp (children node))
						       (> (length (children node)) 1)))
(defmethod unextended-node-p ((node lattice-node)) (null (children node)))

(defun safe-children (node)
  (assert (not (unextended-node-p node)))
  (unless (terminal-node-p node)
    (children node)))

(defun num-children (node)
  (length (safe-children node)))

(defun term-text (node)
  (token-for-id (term node)))

(defun linear-lattice-string (node)
  (concat-words (tokens-for-ids (linear-lattice-terms node)))) 

(defun linear-lattice-terms (node)
  (cons (term node)
	(loop 
	   while (not (terminal-node-p node))
	   for next = (first (children node))
	   collect (progn (setf node next) (term next)))))

;;
;; Make left and right simple trees
;;

(defparameter *forward-lattice-p* t
  "Determines what direction we walk sentences to extend the lattice")

(defun make-right-term-tree (dict source target)
  (let ((root (get-tree-root dict source target)))
    (when root
      (make-all-lattice-subtrees root)
      (walk-tree 'count-leaves root)
      (label-target root target :target)
      root)))

(defun make-left-term-tree (dict source target)
  (let ((*forward-lattice-p* nil))
    (declare (special *forward-lattice-p*))
    (make-right-term-tree dict source target)))

(defparameter *topic* nil)

(defun label-target (root target type)
  (let ((terms (mapcar 'get-lemma (ids-for-tokens (extract-words target)))))
    (labels ((change-type (node)
	       (if (member (get-lemma (term node)) terms)
		   (setf (type node) type)
		   (when *topic*
		     nil)))) ;;(let ((topic (get-topic 
      (walk-tree #'change-type root))))

;;(defun lookup-topic (
;;		     (let* ((sentence (origin-sentence node))
;;			    (msg (message sentence)))
;;		       (when (eq (type-of msg) 'message)
;;			 (nth (+ (origin-offset sentence) (phrase-start sentence))
;;			      (message-topics msg))

(defun make-term-lattice (dict source target)
  (form-lattice (make-right-term-tree dict source target) target))

(defun form-lattice (root string &optional (matcher #'equal))
  "Walk the tree and connect all references that match the broot
   text field to the broot"
  (let ((target nil))
    (labels ((link-to-target (node)
	       (awhen (and (listp (children node))
			   (some #'match-node (children node)))
		 (unless target 
		   (setf target it)
		   (setf (children target) :terminal)
		   (print target))
		 (setf (children node) (list target))))
             (match-node (node)
	       (when (funcall matcher (token-for-id (term node)) string)
		 node)))
      (walk-tree #'link-to-target root))))
;;
;; lattice context manipulation and tree construction
;;

(defparameter *prune-subtrees-p* t
  "Heuristic pruning during subtree construction to emphasize branches")

(defun get-tree-root (dict source target)
  "Given a term, find a list of matching contexts and return nodes
   which contain pointers to the next term (lazy construction)"
  (declare (optimize (speed 3) (safety 1)))
  (let ((root nil))
    (print *topic*)
    (map-locations (lambda (sid offset)
		     (let ((sentence (get-sentence dict sid)))
		       (when (and (or (null target)
				      (sentence-contains-terms sentence target offset 
							       (not *forward-lattice-p*)))
				  (or (null *topic*)
				      (sentence-contains-topic sentence offset)))
			 (let ((new (make-lattice-node (get-sentence dict sid)
						       offset :make-child-p t)))
			   (if root
			       (merge-lattice-nodes root new)
			       (setf root new))))))
		   (word-locations dict source))
    root))

(defun sentence-contains-topic (sentence offset)
  ;; TODO: Fix topic offset
  (chunk-has-topic (message sentence) sentence *topic*))

(defun sentence-contains-terms (sentence terms &optional (offset 0) from-end)
  (declare (optimize (speed 3) (safety 1))
	   (type fixnum offset))
  (let ((doc (document-text (phrase-document sentence)))
	(start (phrase-start sentence))
	(end (phrase-end sentence))
	(ids (mapcar #'id-for-token (extract-words terms))))
;;             (mapcar (compose #'get-lemma-for-id #'id-for-token) (extract-words terms))))
    (declare (type (array fixnum *) doc)
	     (type fixnum start end))
    (if from-end
	(loop for i fixnum from (+ offset start) downto 0
	     when (member (aref doc i) ids)
	     do (return t))
	(loop for i fixnum from (+ offset start) upto end
	     when (member (aref doc i) ids)
	     do (return t)))))

(defun sentence-contains-phrase (sentence query &optional (offset 0) from-end)
  (mvbind (start end)
      (find-phrase (make-phrase-from-sentence query)
		   (phrase-document sentence)
		   :start (if from-end (phrase-start sentence)
			      (+ (phrase-start sentence) offset))
		   :lemma t
		   :ignore-start (if from-end (+ (phrase-start sentence) offset) 
				     (phrase-end sentence))
		   :ignore-end (length-of (phrase-document sentence)))
    (when start
      (values (- start (phrase-start sentence))
	      (- end (phrase-start sentence))))))
	       

(defun make-all-lattice-subtrees (node)
  (unless (terminal-node-p node)
    (make-lattice-subtrees node)
    (mapc #'make-all-lattice-subtrees (children node)))
  node)


(defun make-lattice-subtrees (node)
  (with-slots (children) node
    (assert (not (some #'branching-node-p children)))
    (setf children (get-subtrees (extend-lattice-nodes children)))
    (when *prune-subtrees-p* (prune-subtrees node)))
  node)

(defun all-children-extended-p (node)
  (walk-tree (f (node) 
	       (when (unextended-node-p node)
		 (return-from all-children-extended-p nil)))
	     node)
  t)
  
	     
(defun prune-subtrees (node)
  "Assumes grandchildren have not been collapsed"
  (let* ((total (leaf-count node))
	 (threshold (floor (* total 0.01))))
    (when (> total 100)
      (setf (children node)
	    (filter-if (f (child)
			 (<= (num-children child) threshold))
		       (safe-children node))))
    node))

;; Extended lazy lattice nodes 


(defun completely-extend-lattice-node (node)
  "Extend this node to the end of all sentences"
  (unless (terminal-node-p (extend-lattice-node node))
    (mapc #'completely-extend-lattice-node (children node)))
  node)

(defun extend-lattice-nodes (nodes)
  "Extend all the lattice nodes or indicate that they are terminal"
  (mapc 'extend-lattice-node nodes)
  nodes)

(defun extend-lattice-node (node)
  "Given an unextended node, extend it by one token"
  (with-slots (sentence offset children) node
    (when (unextended-node-p node)
      (setf (children node)
	    (cond ((and *forward-lattice-p* (has-next-token-p sentence offset))
		   (list (make-lattice-node sentence (1+ offset))))
		  ((and (not *forward-lattice-p*) (has-prev-token-p sentence offset))
		   (list (make-lattice-node sentence (1- offset))))
		  (t :terminal)))))
  node)

;; Build subtrees

(defun get-subtrees (nodes)
  "Given a list of nodes, combine nodes that share terms
   by returning a single node merging the children"
  (let* ((sorted (sort (copy-list nodes) #'> :key #'term))
	 (last (first sorted))
	 (results nil))
    (dolist (node (rest sorted))
      (if (node-equal last node)
	  (setf last (merge-lattice-nodes last node))
	  (progn (push last results)
		 (setf last node))))
    (cons last results)))

;; Merge lattice nodes

(defun merge-lattice-nodes (node1 node2)
  "Merge node2's children into the children of node1."
  (declare (type lattice-node node1 node2)
	   (optimize (speed 3) (space 2)))
  (assert (not (or (unextended-node-p node1) (unextended-node-p node2))))
  (setf (children node1) 
	(nconc (safe-children node1) (safe-children node2)))
  (unless (children node1) (setf (children node1) :terminal))
  (setf (leaf-count node1)
	(+ (leaf-count node1) (leaf-count node2)))
  node1)

(defun merge-all-lattice-nodes (nodes)
  "Merge all the nodes into a single node (i.e. for the root node)"
  (reduce 'merge-lattice-nodes (rest nodes) :initial-value (first nodes)))

;; ======================================= 
;; ANNOTATING LATTICES
;; ======================================= 

(defun walk-tree (fn node)
  "Trivial tree walker, assumes tree DAG"
  (mapc (f (child) 
	  (when (unextended-node-p child)
	    (extend-lattice-node child))
	  (walk-tree fn child))
	(safe-children node))
  (funcall fn node)
  node)

(defun count-leaves (node)
  (cond ((branching-node-p node)
	 (progn (setf (leaf-count node) 0)
		(mapc (f (child) (incf (leaf-count node) (leaf-count child)))
		      (safe-children node))))
	((terminal-node-p node)
	 (setf (leaf-count node) 1))
	(t (setf (leaf-count node) (leaf-count (first (children node)))))))

(defun annotate-lattice (annotator root)
  "Walk tree, accumulating subnode counts and calling annotator
   on each node on the way back out."
  (walk-tree annotator root))


;;
;; Sending trees to the client
;;

(defun serialize-lattice (lattice)
  (declare (optimize (speed 3) (safety 1)))
  (let ((id-hash (make-hash-table :test #'eq))
	(obj-hash (make-hash-table :test #'eq))
	(id 0))
    (declare (type hash-table id-hash obj-hash)
	     (type fixnum id))
    (when (null lattice)
      (return-from serialize-lattice '((("id" . 0) ("error" . "No results")))))
    (labels ((node-id (obj)
	       (gethash obj obj-hash))
	     (add-node (obj)
	       (unless (node-id obj)
		 (prog1 (setf (gethash obj obj-hash) id)
		   (incf id)))))
      (add-node lattice) ;; add root node at id = 0
      (walk-tree (f (node)
		   (add-node node)
		   (setf (gethash (node-id node) id-hash)
			 (serialize-lattice-node node #'node-id)))
		 lattice)
      (hash-values id-hash))))

(defun serialize-lattice-node (node idfn)
  `(("id" . ,(funcall idfn node))
    ("name" . ,(token-for-id (term node)))
    ("weight" . ,(leaf-count node))
    ("author" . ,(author (origin-sentence node)))
    ("type" . ,(string-downcase (symbol-name (type node))))
    ("children" . ,(mapcar idfn (safe-children node)))))

(define-api-handler "/wordlattice" (method params json)
  (with-assocs (source match dict max type topics topic) params
    (let ((dictionary (or (lookup-dict dict) (lookup-dict "LAM"))))
      (let ((*max-locations* (when (equal match "") max))
	    (*topic* (unless (and (equal topics "false")
				  (or (equal topic "") (equal topic "NA")
				      (null topic) (and (numberp topic) (< topic 0))))
		       topic))
	    (target (unless (equal match "") match)))
	(declare (special *max-locations* *topic*))
	(serialize-lattice 
	 (cond ((and (equal type "Lattice") target)
		(make-term-lattice dictionary source target))
	       ((equal type "Back")
		(make-left-term-tree dictionary source target))
	       (t (make-right-term-tree dictionary source target))))))))


;;
;; 
;;

(defun read-speechome-file (filename)
  (let ((dict (make-instance 'sentence-dictionary) ))
    (with-open-file (stream filename)
      (do-contentful-lines (line stream :count 10000)
	(add-message dict
	 (make-instance 'email
			:url ""
			:headers ""
			:src ""
			:date ""
			:sender "rupal"
			:subject "language"
			:fwd ""
			:body line))))
    dict))

(defun read-speechome-file2 (filename)
  (let ((dict (make-instance 'sentence-dictionary) ))
    (with-open-file (stream filename)
      (do-contentful-lines (line stream)
	(add-sentence dict (as-sentence (string->vdoc->phrase line)))))
    dict))

(defun string->vdoc->phrase (string)
  (let ((words (ids-for-tokens (extract-words string))))
    (make-phrase (list->array words :type 'fixnum)
		 (make-array (length words) :element-type 'symbol))))
	       
					     
