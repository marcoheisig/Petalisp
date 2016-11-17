;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-reference (strided-array reference) ())

(defmethod reference ((object strided-array)
                      (space strided-array-index-space)
                      (transformation transformation))
  (let ((target-space (transform space transformation)))
    (if (equal? (index-space object) target-space)
        object
        (make-instance
         'strided-array-reference
         :predecessors (list object)
         :ranges (ranges target-space)
         :element-type (element-type object)
         :transformation transformation))))

(defmethod reference ((object strided-array-reference)
                      (space strided-array-index-space)
                      (t2 transformation))
  "Fold references to other references."
  (let ((target-space (transform space t2))
        (transformation (compose t2 (transformation object))))
    (reference
     (first (predecessors object))
     (transform target-space (invert transformation))
     transformation)))

(defmethod reference ((fusion strided-array-fusion)
                      (space strided-array-index-space)
                      (transformation transformation))
  "Make references to fusions reference the inputs of the fusion instead."
  (aif (find space (predecessors fusion) :test #'equal? :key #'index-space)
       (reference it space transformation)
       (call-next-method)))
