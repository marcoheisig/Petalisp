;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod transform ((object strided-array-index-space)
                      (transformation affine-transformation))
  (let ((result (make-array (output-dimension transformation)))
        (ranges (ranges object)))
    (loop for input-constraint across (input-constraints transformation)
          and range across ranges do
          (when input-constraint
            (assert (and (unary-range? range)
                         (= (range-start range)
                            input-constraint)))))
    (loop for p across (permutation transformation)
          and i from 0 do
            (setf (aref result i)
                  (let ((a (aref (affine-coefficients transformation) i 0))
                        (b (aref (affine-coefficients transformation) i 1))
                        (x (or (and p (aref ranges p))
                               (range 0 1 0))))
                    (range (+ (* a (range-start x)) b)
                           (* (range-step x) a)
                           (+ (* (range-end x) a) b)))))
    (make-instance
     'strided-array-index-space
     :ranges result)))
