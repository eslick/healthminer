(in-package :smart)



;; ==============================================================
;;  Match templates 
;; ==============================================================

(defun template-terms (template)
  (select-if #'atom template))

(defun convert-result (result)
  (mapcar (lambda (term) 
	    (typecase term
	      (number (token-for-id term))
	      (string term)))
	  result))

(defun message-template-matches (template)
  (loop for msg in (messages-for-words (template-terms template)) 
       for matches = (template-match template (message-lemmas msg))
       nconc matches))

(defun template-match (template terms)
  (loop for position in (term-positions (get-lemma (first template)) terms)
     for match = (template-match-next nil (rest template) (nthcdr (1+ position) terms) 
				      (list (first template)))
     when match collect match))

(defun template-match-term-head (template terms)
  (eq (get-lemma-for-id (first template)) (first terms)))

(defun template-match-next (state template terms matched)
  (cond ((null template) 
	 (reverse matched))
	(state
	 (template-match-next-op state template terms matched))
	((not state)
	 (template-match-next-term template terms matched))
	(t (error "Unknown state for template matcher: ~A ~A" template terms))))

(defun template-match-next-op (state template terms matched)
  (ecase (first state)
    (* (unless (< (second state) 0)
	   (cond ((next-is-member template)
		  (template-match-next-member state template terms matched))
		 ((template-match-term-head template terms)
		  (template-match-next nil (rest template) (rest terms)
				       (cons (first terms) matched)))
		 (t (template-match-next-op `(* ,(1- (second state))) template (rest terms) 
					    (cons (first terms) matched))))))
    (member (unless (null (cdr state))
	      (if (template-match-term-head (cdr state) terms)
		  (template-match-next nil template (rest terms) (cons (first terms) matched))
		  (template-match-next-op (cons 'member (cddr state))
					  template terms matched))))))

(defun template-match-next-term (template terms matched)
  (dbind (current . rest) template
    (if (listp current)
	(template-match-next current rest terms matched)
	(when (template-match-term-head template terms)
	  (template-match-next nil rest (rest terms) (cons (first terms) matched))))))

(defun template-match-next-member (state template terms matched)
  (aif (template-match-next (first template) nil terms matched)
       (template-match-next nil (cdr template) (rest terms) (cons (first terms) matched))
       (template-match-next-op `(* ,(1- (second state))) template (rest terms)
			       (cons (first terms) matched))))

(defun next-is-member (template)
  (and (listp (first template)) (eq 'member (caar template))))


;;
;; Extract template instances
;;

(defun all-message-template-matches (template)
  (remove-nulls 
   (flatten1 
    (map-messages (id rec)
      (template-match template (mmap-lemmas rec))))))

;; ==============================================================
;;  Lexical templates
;; ==============================================================

;; (:fluid likes to collect :location)
;; (P "test" "foo" * "bar")



