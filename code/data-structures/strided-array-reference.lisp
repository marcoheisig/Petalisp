;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-reference (strided-array reference) ())

(defmethod reference ((object strided-array)
                      (space strided-array-index-space)
                      (transformation transformation))
  (make-instance
   'strided-array-reference
   :predecessors (list object)
   :index-space space
   :element-type (element-type object)
   :transformation transformation))

(defmethod reference ((object strided-array)
                      (space strided-array-index-space)
                      (transformation identity-transformation))
  (if (equal? (index-space object) space)
      object
      (call-next-method)))

(defmethod reference ((object strided-array-reference)
                      (space strided-array-index-space)
                      (t2 transformation))
  "Fold references to other references."
  (let ((target-space (funcall t2 space))
        (transformation (composition t2 (transformation object))))
    (reference
     (first (predecessors object))
     (funcall (inverse transformation) target-space)
     transformation)))

(defmethod reference ((fusion strided-array-fusion)
                      (space strided-array-index-space)
                      (transformation transformation))
  "Make references to fusions reference the inputs of the fusion instead."
  (if-let (it (find space (predecessors fusion) :test #'equal? :key #'index-space))
    (reference it space transformation)
    (call-next-method)))
