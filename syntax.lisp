(in-package :smart)

;;(declaim (optimize (safety 3) (debug 3) (optimize 1)))

;;
;; Manipulations of parse tree
;;

;; Leaves

(defun tree-leaf-p (tree)
  (and (listp tree) (every (f (elt) (not (tree-node-p-list-aux elt)))
			   (subseq tree 0 (min 4 (length tree))))))

(defun tree-leaf-pos (leaf) (first leaf))
(defun tree-leaf-id (leaf) (second leaf))
(defun tree-leaf-word (leaf) (third leaf))
(defun tree-leaf-lemma (leaf) (fourth leaf))
(defun tree-leaf-ne (leaf) (fifth leaf))
(defun tree-leaf-morphology (leaf) (nthcdr 5 leaf))

;; Nodes

(defun tree-node-p (tree)
  (and (listp tree) 
       (or (argument-node-p tree)
	   (some #'tree-node-p-list-aux (subseq tree 0 (min 4 (length tree)))))))

(defun tree-node-p-list-aux (elt)
  (and (listp elt) (not (eq (first elt) 'quote))))

(defun argument-node-p (tree)
  (or (unconverted-argument-node-p tree)
      (converted-argument-node-p tree)))

(defun unconverted-argument-node-p (tree)
  (eq (third tree) '{))

(defun converted-argument-node-p (tree)
  (or (eq (third tree) :arg)
      (eq (third tree) :theme)))

(defun tree-node-type (node) (first node))
(defun tree-node-id (node) (second node))
(defun tree-node-arglink (node)
  (when (argument-node-p node)
    (fourth node)))
(defun tree-node-constituents (node)
  (when (tree-node-p node)
    (if (argument-node-p node)
	(if (converted-argument-node-p node)
	    (nthcdr 4 node)
	    (nthcdr (1+ (position '} node)) node))
	(cddr node))))

;; Arguments

(defun convert-argument-node (node)
  (when (unconverted-argument-node-p node)
    (mvbind (arguments rest)
	(convert-arguments node)
      (setf (third node) :arg)
      (setf (fourth node) arguments)
      (setf (nthcdr 4 node) rest))
    node))

(defun convert-arguments (node)
  (let* ((start (position '{ node))
	 (end (position '} node))
	 (arguments (subseq node (1+ start) end)))
    (values (mapcar #'convert-argument-symbol arguments)
	    (subseq node (1+ end)))))

(defun convert-argument-symbol (sym)
  (when (node-arg-p sym)
    (mapcar #'read-from-string
	    (split-alphanumeric-string 
	     (symbol-name sym)))))

;; Semantic annotation linkage

(defun node-arg-p (arg)
  (eq (char (string-downcase (symbol-name (if (consp arg) (first arg) arg))) 0) #\b))

(defun node-arg-type (arg)
  (when (node-arg-p arg)
    (subseq arg 1 (1- (length arg)))))

(defun node-arg-verb-id (arg)
  (when (node-arg-p arg)
    (last1 arg)))

;; Generic Tree Utilities

(defun walk-tree (node-fn leaf-fn tree)
  (labels ((walker (node)
	     (cond ((tree-node-p node)
		    (funcall node-fn 
			     node
			     (mapcar #'walker
				     (tree-node-constituents node))))
		   ((tree-leaf-p node)
		    (funcall leaf-fn node))
		   (t (error "Unrecognized parse tree element: ~A" node)))))
    (walker tree)))

(defun map-leaves (leaf-fn tree)
  "Walks the tree applying leaf-fn and making a linear list of 
   those returned values using append"
  (walk-tree (lambda (node subs)
	       (declare (ignore node))
	       (apply #'append subs))
	     (lambda (leaf)
	       (funcall leaf-fn leaf))
	     tree))

(defun select-leaves (select-fn tree)
  (walk-tree (lambda (n s)
	       (declare (ignore n))
	       (remove-nulls (apply #'append s)))
	     (lambda (l)
	       (aif-ret (list (funcall select-fn l)) (list l)))
	     tree))

(defun filter-leaves (filter-fn tree)
  (select-leaves (lambda (value)
		   (unless (funcall filter-fn value)
		     value))
		 tree))

;; Other useful ops

(defun get-node-by-path (path tree &optional id)
  (assert (consp path))
  (when (eq (tree-node-type tree) (first path))
    (if (= (length path) 1)
	(when (or (null id) (eq (tree-node-id tree) id))
	  tree)
	(first (remove-nulls 
		(mapcar #'(lambda (subtree)
			    (get-node-by-path (rest path) subtree id))
			(tree-node-constituents tree)))))))

(defun simple-tree (tree)
  (walk-tree (lambda (node subs)
	       (append (list (tree-node-type node)) subs))
	     (lambda (leaf)
	       (list (tree-node-type leaf) (tree-leaf-word leaf)))
	     tree))

(defun get-tree-leaves (tree)
  (select-leaves #'identity tree))

(defun tree-words (tree)
  (select-leaves #'tree-leaf-word tree))

(defun leaf-word-pos-pair (leaf) (list (leaf-word-pos leaf)))
(defun tree-word-pos-pairs (tree)
  (map-leaves #'leaf-word-pos-pair tree))

(defun leaf-word-pos (leaf) (list (tree-leaf-word leaf) (tree-leaf-pos leaf)))
(defun tree-word-pos-sequence (tree)
  (map-leaves #'leaf-word-pos tree))

(defun leaf-if-verb (leaf)
  (when (verb-leaf-p leaf) leaf))
(defun verb-leaf-p (leaf)
  (verb-pos-p (tree-leaf-pos leaf)))
(defun noun-leaf-p (leaf)
  (noun-pos-p (tree-leaf-pos leaf)))

(defun tree-verbs (tree)
  (select-leaves #'leaf-if-verb tree))

(defun tree-arguments (tree)
  (walk-tree (lambda (n s)
	       (if (argument-node-p n)
		   (apply #'append (list n) s)
		   (apply #'append s)))
	     (lambda (l) (declare (ignore l)))
	     tree))

(defun find-phrase (type verb tree &optional (node-select #'identity))
  (walk-tree (lambda (n s)
	       (print s)
	       (cond ((and (eq (first n) type) (not (every #'null s)))
		      n)
		     ((not (every #'null s))
		      (first (remove-nulls s)))
		     (t nil)))
	     (lambda (leaf)
	       (print leaf)
	       (when (and (eq (tree-leaf-word leaf) verb)
			  (funcall node-select leaf))
		 leaf))
	     tree))

(defun find-phrase-from-words (type words tree &optional (leaf-select #'identity))
  (mapc #'(lambda (x) (aif (find-phrase type x tree leaf-select)
			   (return-from find-phrase-from-words it)))
	words)
  nil)

(defun find-vp-from-words (words tree)
  (mapc #'(lambda (x) (aif (find-phrase 'vp x tree #'verb-leaf-p)
			   (return-from find-vp-from-words it)))
	words)
  nil)

(defun extract-events (tree)
  (mapcar #'tree-words (extract-event-trees tree)))

(defun extract-event-trees (tree)
  (unless (tree-leaf-p tree)
    (aif-ret (extract-events-aux tree)
      (when (and (tree-node-p tree) (eq (tree-node-type tree) 'vp))
	(list tree)))))

(defun extract-events-aux (tree)
  (mapcan (f (const) (extract-event-trees const))
	  (tree-node-constituents tree)))

;;
;; Annotate verb properties (tense and aspect via tags & patterns)
;;

(defun annotate-verb-properties (tree)
  (let ((verb-nodes (tree-verbs tree))
	(wp-pairs (tree-word-pos-pairs tree)))
    (loop 
       for leaf in verb-nodes 
       unless (leaf-annotated-p leaf) do
	 (let ((word (tree-leaf-word leaf)))
	   (rplacd (nthcdr 4 leaf) (find-verb-properties word wp-pairs))))
    tree))

(defun leaf-annotated-p (leaf)
  (>= (length leaf) 6))

(defmacro define-tense-pattern (pattern &rest features)
  `'(,(nreverse pattern) ,@features))

(defmacro define-tense-patterns (&rest pattern-list)
  `(list ,@(mapcar #'(lambda (pattern)
		       `(define-tense-pattern ,@pattern))
		   pattern-list)))

(defparameter *verb-feature-patterns*
  (define-tense-patterns 
      (((:verb VBP))              present)
      (((:verb VBZ))              present singular)
      (((will *) * (:verb VB))    future)
      (((has AUX) * (:verb VBN))    present perfect)
      (((had AUX) * (:verb VBN))    past perfect)
      (((will *) (have AUX) * (:verb VBN))  future perfect)
      (((has AUX) (:verb VBD))    present perfect) ;; bigram fix
      (((had AUX) (:verb VBD))    past perfect)    ;; bigram fix for common mistags
      (((will *) (have AUX) (:verb VBD))  future perfect) ;; bigram fix for common mistags
      (((:verb VBD))              past)
      (((is AUX) * (:verb VBG))   present progressive)
      (((was AUX) * (:verb VBG))  past progressive)
      (((will *) (be AUX) * (:verb VBG)) future progressive)
      (((has AUX) (been AUX) * (:verb VBG)) present perfect progressive)
      (((had AUX) (been AUX) * (:verb VBG)) past perfect progressive)
      (((will *) (have AUX) (been AUX) * (:verb VBG)) future perfect progressive)))
     
(defun find-verb-properties (verb wp-pairs)
  (labels ((match-pattern (pattern pairs)
	     (cond ((null pattern) t)
		   ((null pairs) nil)
		   ((eq (first pattern) '*)
		    (if (matching-pairs-p verb (second pattern) (first pairs))
			(match-pattern (cddr pattern) (cdr pairs))
			(match-pattern pattern (cdr pairs))))
		   ((matching-pairs-p verb (first pattern) (first pairs))
		    (match-pattern (cdr pattern) (cdr pairs)))
		   (t (match-pattern pattern (cdr pairs))))))
    (let ((rpairs (reverse wp-pairs)))
      (awhen (find t *verb-feature-patterns* :key #'first 
		   :test #'(lambda (ignore pattern)
			     (declare (ignore ignore))
			     (match-pattern pattern rpairs)))
	(cdr it)))))
		 
(defun matching-pairs-p (verb pattern-pair actual-pair)
  (flet ((word-match-p (token actual)
	   (or (and (eq token :verb) (eq actual verb))
	       (eq token '*)
	       (eq token actual)))
	 (pos-match-p (pos actual)
	   (or (eq pos '*)
	       (eq pos actual))))
    (and (word-match-p (first pattern-pair) (first actual-pair))
	 (pos-match-p (second pattern-pair) (second actual-pair)))))

;;
;; Auxiliary syntax information
;;

(defun verb-pos-p (pos)
  (member pos '(VB VBZ VBN VBD VBG :VB :VBZ :VBN :VBD :VBG)))

(defun noun-pos-p (pos)
  (member pos '(NN NNP NNS :NN :NNP :NNS)))

(defun anaphor-p (word)
  (member word '(it he she them they him her this that)))

;;
;; Syntax interface
;;

(defun parse-string (text)
  (handler-case 
      (annotate-verb-properties
       (pre-process-tree
	(swirl-parse-string text)))
    (error () (wait-for-prompt 20))))


(defun pre-process-tree (tree)
  "Clean up tree representation for easier parsing"
  (walk-tree #'(lambda (node constituents) 
		 (declare (ignore constituents))
		 (convert-argument-node node))
	     #'identity
	     tree)
  tree)

;;
;; Template extraction
;;

(defvar *templates* nil)

(defun extract-templates (tree)
  (remove-nulls
   (loop for template in *templates* collect
	(cond ((eq (first template) 'linear)
	       (awhen (unify-linear-template tree (third template))
		 (subst-bindings it (fourth template))))
	      ((eq (first template) 'constituent)
	       (awhen (unify-constitutent-template tree (third template))
		 (subst-bindings it (fourth template))))
	      (t (error "Unrecognized template type: ~A" (first template)))))))

(defmacro define-linear-template (name pattern script)
  `(eval-when (:eval-toplevel :load-toplevel) 
     (pushrpl (list 'linear ',name ',pattern ',script)  *templates* :key #'second)))

(defparameter *punctuation-symbols* '(? ! |.| |,| |;| |:| |)| |(| ))

(defun unify-linear-template (tree template)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((bindings nil))
    (labels ((push-binding (bind)
	       (setf (cdr bind) (nreverse (cdr bind)))
	       (push bind bindings))
	     (search (tp leaves bind)
	       (cond ((null leaves) 
		      (when (< (length tp) 2)
			(push-binding bind)))
		     ((member (first leaves) *punctuation-symbols*)
		      (if (member (first tp) *punctuation-symbols*)
			  (search (rest tp) (rest leaves) bind)
			  (search tp (rest leaves) bind)))
		     ((eq (first tp) (tree-leaf-word (first leaves)))
;;		      (format t "match ~A~%" (first tp))
		      (when bind 
			(push-binding bind))
		      (search (rest tp) (rest leaves) nil))
		     ((not (null bind))
;;		      (format t "push word: ~A~%" (tree-leaf-word (first leaves)))
		      (push (tree-leaf-word (first leaves)) (cdr bind))
		      (search tp (rest leaves) bind))
		     ((single-variable? (first tp))
		      (search (rest tp) (rest leaves)
			      (push-binding (cons (first tp) (first leaves)))))
		     ((variable? (first tp))
;;		      (format t "variable ~A~%" (first tp))
		      (search (rest tp) (rest leaves)
			      (cons (first tp) (list (tree-leaf-word (first leaves))))))
		     (t (setf bindings nil) nil))))
      (declare (dynamic-extent (function search)))
      (cond ((eq (first template) 'or)
	     (setf bindings
		   (some #'(lambda (ct) (unify-linear-template tree ct))
			 (rest template))))
	    (t (search template (get-tree-leaves tree) nil)))
      bindings)))

(defun single-variable? (sym)
  (eq (char (symbol-name sym) 0) #\%))

(defmacro define-constituent-template (name pattern script)
  `(eval-when (:eval-toplevel :load-toplevel)
     (pushrpl (list 'constituent ',name ',pattern ',script) *templates* :key #'second)))

(defun unify-constituents (tree template)
  "Extract matching constituents from a parse tree"
  (error "Not implemented"))