;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-reduction (strided-array reduction) ())

(defun all-but-last (sequence)
  (subseq sequence 0 (1- (length sequence))))

(defmethod reduction ((operator function) (object strided-array))
  (make-instance
   'strided-array-reduction
   :operator operator
   :element-type (element-type object)
   :predecessors (list object)
   :ranges (all-but-last (ranges object))))

(defmethod reduction ((operator function) (object strided-array-constant))
  (if (<= (size object) *constant-fold-threshold*)
      (evaluate-node (call-next-method)) ; constant folding
      (call-next-method)))

#+nil
(defmethod evaluate-node ((node strided-array-reduction))
  (let ((args (mapcar (compose #'data #'evaluate-node) (predecessors node)))
        (op (operator node))
        (result (make-array
                 (map 'list #'size (ranges node))
                 :element-type (element-type node))))
    (make-instance
     'strided-array-constant
     :data result
     :element-type (element-type node)
     :ranges (ranges node))))
