;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-reduction (strided-array reduction) ())

(defmethod reduction ((operator function) (object strided-array))
  (let ((input-ranges (ranges object)))
    (let ((ranges (subseq input-ranges (- (length input-ranges) 2))))
      (make-instance
       'strided-array-reduction
       :operator operator
       :predecessors (list object)
       :ranges ranges))))
