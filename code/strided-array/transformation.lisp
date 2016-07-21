;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-transformation (strided-array transformation) ())

(defmethod transformation ((object strided-array)
                           &key scaling translation permutation)
  (let ((dimension (dimension object)))
    (let ((ranges
            (ranges object))
          (scaling
            (or scaling (make-list dimension :initial-element 1)))
          (translation
            (or translation (make-list dimension :initial-element 0)))
          (permutation
            (or permutation (iota dimension))))
      (assert (= dimension
                 (length scaling)
                 (length translation)
                 (length permutation)))
      (make-instance
       'strided-array-transformation
       :object object
       :ranges
       (mapcar
        (lambda (a b p)
          (let ((r (nth (1- p) ranges)))
            (range (+ (* (range-start r) a) b)
                   (* (range-step r) a)
                   (+ (* (range-end r) a) b))))
        scaling translation permutation)))))
