;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmethod transform ((object strided-array-index-space)
                      (transformation affine-transformation))
  (let ((ranges (make-array (dimension object))))
    (loop for p across (permutation transformation)
          and i from 0 do
            (setf (aref ranges i)
                  (let ((a (aref (affine-coefficients transformation) i 0))
                        (b (aref (affine-coefficients transformation) i 1))
                        (x (or (aref (ranges object) p)
                               (range 0 1 0))))
                    (range (+ (* a (range-start x)) b)
                           (* (range-step x) a)
                           (+ (* (range-end x) a) b)))))
    (make-instance
     'strided-array-index-space
     :ranges ranges)))
