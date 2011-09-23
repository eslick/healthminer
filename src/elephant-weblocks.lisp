(defpackage #:weblocks-elephant
  (:use :cl :weblocks :weblocks-memory))

(in-package :weblocks-elephant)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open and close
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *estore* nil)

(defun open-elephant-store (directory &rest args)
  "Opens a BDB elephant data store."
  (declare (ignore args))
  (elephant:open-store (list :BDB directory)))


(defmethod open-store ((store-type (eql :elephant)) &rest args)
  "Would be better if the directory could be obtained explicitly."
  (if (null *estore*) ;; we open only one store
      (setf *estore* (apply #'open-elephant-store args))
      *estore*))


;; This seems to be the best way to specialize
(defmethod close-store ((store db-bdb::bdb-store-controller))
  (if (eql *estore* store) ;; make sure it is our store 
      (progn
        (elephant:close-store store)
        (setf *estore* nil))
      nil))


;; TODO
(defmethod clean-store ((store db-bdb::bdb-store-controller))
  "Elephant does not seem to have a way to delete a store. Maybe just delete the store's root?"
  (declare (ignore store)))


(defmethod supports-filter-p ((store db-bdb::bdb-store-controller))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transactions
;;
;; Elephant uses the ensure-transaction macro to wrap transactions.
;; These methods, they do nothing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod begin-transaction ((store db-bdb::bdb-store-controller))
  nil)

(defmethod commit-transaction ((store db-bdb::bdb-store-controller))
  nil)

(defmethod rollback-transaction ((store db-bdb::bdb-store-controller))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; CRUD methods
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; In elephant, you don't explicitly store objects
(defmethod persist-object ((store db-bdb::bdb-store-controller) object)
  (declare (ignore store object)))


;; Can't delete them either...this is weird
(defmethod delete-persistent-object ((store db-bdb::bdb-store-controller)
                                     object)
  (elephant:drop-instances (list object)))


(defmethod delete-persistent-object-by-id ((store db-bdb::bdb-store-controller) 
                                           class-name object-id)
  (declare (ignore class-name))
  (elephant:drop-instances
   (list (elephant::controller-recreate-instance store object-id))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search functions
;; Here elephant provides many more/better methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun find-persistent-object-by-slot-value (class-name slot-name slot-value)
  "The slots of the class should be indexed for this to work."
  (elephant:get-instance-by-value class-name slot-name slot-value))

(defun find-persistent-objects-by-slot-value (class-name slot-name slot-value)
  "The slots of the class should be indexed for this to work."
  (elephant:get-instances-by-value class-name slot-name slot-value))

(defun find-persistent-objects-by-slot-range (class-name slot-name lower-bound upper-bound)
  "The slots of the class should be indexed for this to work."
  (elephant:get-instances-by-range class-name slot-name lower-bound upper-bound))

(defun map-instances (function class-name &key (collect nil))
  (elephant:map-class function class-name :collect collect ))

(defun find-persistent-objects-by-class (class-name)
  (elephant:get-instances-by-class class-name))

(defmethod find-persistent-objects ((store db-bdb::bdb-store-controller) 
                                    class-name 
                                    &key filter filter-args order-by range)
  (range-objects-in-memory
   (order-objects-in-memory
    (filter-objects-in-memory
     (elephant:get-instances-by-class class-name)
     filter filter-args)
    order-by)
   range))


(defmethod count-persistent-objects ((store db-bdb::bdb-store-controller) 
                                     class-name &key filter filter-args order-by range)
  (length (find-persistent-objects store class-name
                                   :filter filter
                                   :order-by order-by
                                   :range range
                                   :filter-args filter-args)))


(defun count-persistent-objects-by-class (class-name)
  (let ((count 0))
    (map-instances (lambda (obj)
                     (declare (ignore obj))
                     (incf count))
                   class-name)
    count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test - run from repl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(in-package :weblocks-elephant)

(elephant:defpclass friend ()
  ((name :accessor name :initarg :name :index t)
   (age :accessor age :initarg :age :index t)))

(defun make-new ()
  (list 
   (make-instance 'friend
                  :name "Raja"
                  :age 1)
   (make-instance 'friend
                  :name "Meera"
                  :age 5)
   (make-instance 'friend
                  :name "Nithya"
                  :age 5)
   (make-instance 'friend
                  :name "Neha"
                  :age 2)
   (make-instance 'friend
                  :name "Ella"
                  :age 0)))
|#