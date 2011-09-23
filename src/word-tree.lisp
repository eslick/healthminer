(in-package :smart)

;; File contains reps & processing to compute the essential data 
;; behind the Word Tree data visualization

(defun get-word-tree (mstream terms)
  "Return the left and right common prefix tree
   for a given term in a given media stream"
  (declare (ignore terms))
  (let ((windows (all-term-filtered-windows terms 40)))
    

;; word: list of sentences it partakes in
;; sentence: left/right sentence links
;; prefix tree: given list of sentences
;;      

