;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; (broadcast #i((1 1 2) (2 3 100)) #i((0 1 0)))
(defmethod broadcast ((space-1 strided-array)
                      (space-2 strided-array))
  (when (< (dimension space-1) (dimension space-2))
    (rotatef space-1 space-2))
  (let ((ranges-1 (ranges space-1))
        (ranges-2 (ranges space-2))
        (dim-1 (dimension space-1))
        (dim-2 (dimension space-2)))
    (make-instance
     'strided-array-index-space
     :ranges
     (concatenate
      'vector
      (map 'vector #'broadcast ranges-1 ranges-2)
      (subseq
       (if (< dim-1 dim-2) ranges-2 ranges-1)
       (min dim-1 dim-2))))))

(defmethod broadcast ((range-1 range) (range-2 range))
  (let ((u1 (unary-range-p range-1))
        (u2 (unary-range-p range-2)))
    (cond
      ((equalp range-1 range-2) range-1)
      ((and u1 (not u2)) range-2)
      ((and (not u1) u2) range-1)
      (t (error "No upgradeable ranges.")))))
