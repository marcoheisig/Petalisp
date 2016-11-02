;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod broadcast ((space-1 strided-array)
                      (space-2 strided-array))
  (when (>= (dimension space-1) (dimension space-2))
    (rotatef space-1 space-2))
  (let ((ranges-1 (ranges space-1))
        (ranges-2 (ranges space-2)))
    (make-instance
     'strided-array-index-space
     :ranges
     (concatenate
      'vector
      (map 'vector #'broadcast ranges-1 ranges-2)
      (subseq ranges-2 (length ranges-1))))))

(defmethod broadcast ((range-1 range) (range-2 range))
  (cond
    ((unary-range? range-1) range-2)
    ((unary-range? range-2) range-1)
    ((equalp range-1 range-2) range-1)
    (t (error "Ranges not upgradeable."))))
