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
  (when (> (size object) *constant-fold-threshold*)
    (return-from reduction (call-next-method)))
  ;; now the actual constant folding
  (call-next-method)) ; TODO


