(in-package :statistics)

;;Outlier detection function, hopefully useful.
;;first pass--O(n^2) running time, horrendously inefficient, etc. etc. 
;;Probably use other one (if it works)

(defun unified-detect-outliers-naive (p d distance-measure identifier-list &rest numseqs)
  "Horribly inefficient O(n^2) implementation of unified outlier detection.
   distance-measure is a function that should take in two lists which have as many elements  
   as there are numseqs provided (all numseqs provided should be the same length).
   First list consists of one endpoint, second 
   list composed of coordinates of other endpoint
   Identifier-list is a list, numseqs are lists"
(let ((list-of-outliers '())
      (threshold (* p (length identifier-list))))
  ;(assert (length identifier-list) (if (null (numseqs))
  ;					   (progn 0)
;					   (length (car numseqs))))
  
  (loop for i from 0 to (1- (length identifier-list)) do 
     ;; get the startpoints
       (let ((first-endpoint (map 'list (lambda (numseq)  
		      (nth i numseq)) 
		     numseqs))
	     (num-distant 0))
	 
	 (loop for j from 0 to (1- (length identifier-list)) do
		(unless (= i j)
		  (let ((distance (funcall distance-measure first-endpoint (map 'list (lambda (numseq)
											  (nth j numseq))
											  numseqs))))
		 
		    (when (>= distance d)
		      (incf num-distant)
		      (when (>= num-distant threshold)
			(push (nth i identifier-list) list-of-outliers)
			(return)))
		    
		    )))))
  list-of-outliers
  ))

;;This example shows how to detect outliers in the list '(5 1 2 3 4 1 2 3 4 6 200) (repeated twice because one 
;; is just an identifier, the other contains the data) with a default euclidean distance measure. P = .88, and D = 100.
;;Should return 200 
(unified-detect-outliers-naive .88 100 (lambda (lstx lsty) (abs (- (car lstx) (car lsty)))) '(5 1 2 3 4 1 2 3 4 6 200) '(5 1 2 3 4 1 2 3 4 6 200))
 
;;much more efficient algorithm used here (O(n)), but only works for euclidean distance measures TODO: Finish auxiliary functions
(defun unified-detect-outliers (p d identifier-list &rest numseqs)
  (let* ((dimension (length numseqs))
	(list-of-outliers '())
	(threshold (* p (length identifier-list)))
	(m (- (length identifier-list) threshold))
	(cell-length (/ d (* 2 (sqrt (length numseqs)))))
	(cells-objects (make-hash-table))
	(cells-count (make-hash-table))
	(colored-cells (make-hash-table))
	(max-min-list '()))
    (flet ((distance (end1-list end2-list) 
	     (sum-list (mapcar (lambda (x1 x2)
				 (sqr (- x1 x2))
				 )
			       end1-list end2-list))))
      ;; need to set up cell structure
      ;;treat each numseq in numseqs as a dim
      (setf max-min-list (map 'list (lambda (numseq) 
				      (let ((max most-negative-fixnum)
					    (min most-positive-fixnum))
					(map 'list (lambda (num)
						     (when (< num min)
						       (setf min num))
						     (when (> num max)
						       (setf max num))
						     )
					     numseq)
				      (list min max))
				      )
			      numseqs))
      ;;finished setting up the cell info needed
      ;; divide into grid of cellz?
      (let ((cnt 0))
	(apply #'map 'length (lambda (&rest coords)
			       (let ((cell (coords-to-cells coords max-min-list cell-length)))
				 (push-nil cnt (gethash cell cells-objects))
				 (incf-nil (gethash cell cells-count))
				 )
			       (incf cnt)
			       )
	       numseqs
	       )
	)
      ;; N.B. The data structures may be more complicated than needed. From what I could glean from the paper,
      ;; it seems that they only need to look at cells that are neither "red" nor "pink", so a simple list or something
      ;; would suffice. However, they put great emphasis on labeling cells "red" and "pink" so there might be something 
      ;; I'm missing. Chose to err on the side of caution and flexibility. 
      
      ;;cells initialized; now do hard work
      ;;step 3 & 4
      (maphash (lambda (key val)
		 (when (> val m)
		   (setf (gethash key colored-cells) :red)
		   (remhash key cells-objects) ;; only line needed if red/pink distinction insignificant
		   ;;check L1 neigbors, label them pinkish
		   (mapcar (lambda (cell)
			     (unless (eq (gethash cell colored-cells) :red)
			       (setf (gethash cell colored-cells) :pink)
			       (remhash key cells-objects))) ;; only line needed if red/pink distinction insignificant
			   (lvl1neighbors key dimension max-min-list cell-length)) 
		   )
		 )
	       cells-count 
	       )
      
      (maphash (lambda (cell objects-list)
		 (let ((count2 (apply #'+ (gethash cell cells-count)
				      (list-of-counts (lvl1neighbors cell dimension max-min-list cell-length) cells-count)
				      )))
		   (unless (> count2 m)
		     ;;this is count3
		     (if (<= (apply #'+ count2 (list-of-counts (lvl2neighbors cell dimension max-min-list cell-length) cells-count)) m)
			 ;; all objects in cell outliers
			 (append objects-list list-of-outliers)
			 ;;else process object by object 
			 (loop for objectp in objects-list do
			      (let ((countp count2))
				(loop for objectq in (list-of-objects (lvl2neighbors cell dimension max-min-list cell-length) cells-objects) do
				     (when (<= (distance (nth-multiple-lists objectp numseqs) 
							 (nth-multiple-lists objectq numseqs))
					       d)
				       (incf countp)
				       (when (> countp m)
					 (return)))
				     )
				(when (<= countp m)
				  (push objectp list-of-outliers))
				))
			 )
		     )
		   )
		 )
	       cells-objects
	       )
      list-of-outliers
      ))
  )
  
;;maybe merge this function with above if it doesn't prove useful elsewhere
;;returns a 'cell', which is a list of length (max-min-list), consisting of the row,
;;column etc. of the cell. For example, in the 
;;2D case, cell might be the list (1 4), which would designate a cell
;;in row 1 and column 4.
(defun coords-to-cells (list-of-coordinates max-min-list cell-length)
  ;;determine the number of cells in either dimension
  ;;main function
  (mapcar (lambda (coord max-min)
	    (ceiling (+ (/ (- coord (car max-min)) cell-length) 0.000000000001)) ;; guarantees that cell 0 is empty
	    )
	  list-of-coordinates max-min-list)
  )

;;returns list of lvl1 neighbors of this cell
(defun lvl1neighbors (cell dimension max-min-list cell-length)
  ;;following mapcar produces a list with the number of cells in each dimension
  ;;TODO: move this inside main function. no need to calculate it again all the time
  (let ((num-cell-list (mapcar (lambda (max-min)
			       (let ((min (car max-min))
				     (max (cadr max-min)))
				 (ceiling (/ (- max min) cell-length)) 	      
				 )
			       )
			     max-min-list)))
    
    )
  )
;;returns list of lvl2 neighbors of this cell
(defun lvl2neighbors (cell dimension max-min-list cell-length)

  )

(defun incf-nil (x)
  (if (null x)
      (setf x 0)
      (incf x)))

(defun push-nil (x place)
  (if (null place)
      (setf place (list x))
      (push x place)))

(defun list-of-counts (cells cell-to-count-table)
  (mapcar (lambda (cell)
	    (gethash cell cell-to-count-table) ;; get count
	    )
   cells))

(defun list-of-objects (cells cell-to-objects-table)
    (mapcar (lambda (cell)
	    (gethash cell cell-to-objects-table) ;; get objects
	    )
   cells))

(defun sum-list (lst)
  (if lst
      (+ (car lst) (sum-list (cdr lst)))
    0))

(defun nth-multiple-lists (index &rest lists)
  (mapcar (lambda (lst)
	    (nth index lst))
	  lists))