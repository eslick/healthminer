(in-package :smart)

;; =====================================
;; Relational Exp. Max
;; =====================================

;;;; Training
;; Given a set of known relations and constituents
;; Train model to classify contexts (features?)
;; Anything not labeled is negative?  (overfit)
;; Training only on positive examples (underfit)

;;;; Bootstrapping
;; EM to train descriminator given small set of labels and 
;;   pairs w/ unlabeled data

;;;; Labeling
;; Given contexts w/ pairs of concepts
;; Identify type of relation expressed