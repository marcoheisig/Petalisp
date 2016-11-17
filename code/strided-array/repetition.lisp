;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-repetition (strided-array repetition) ())

(defmethod repetition ((object strided-array)
                       (space strided-array))
  (repetition object (index-space space)))

(defmethod repetition :before ((object strided-array)
                               (space strided-array-index-space))
  (assert (every (lambda (range-1 range-2)
                   (zerop (rem (size range-2) (size range-1))))
                 (ranges object)
                 (ranges space))))

(defmethod repetition ((object strided-array)
                       (space strided-array-index-space))
  (cond ((equal? (index-space object) space) object)
        (t (make-instance
            'strided-array-repetition
            :predecessors (list object)
            :element-type (element-type object)
            :ranges (ranges space)))))
