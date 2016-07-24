;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-reference (strided-array reference) ())

(defmethod transform ((space strided-array-index-space)
                      &key scaling translation permutation)
  (make-instance
   'strided-array-index-space
   :ranges
   (let ((ranges (ranges space)))
     (mapcar
      (lambda (scaling translation permutation)
        (let ((range (nth (1- permutation) ranges)))
          (range (+ (* (range-start range) scaling) translation)
                 (* (range-step range) scaling)
                 (+ (* (range-end range) scaling) translation))))
      scaling translation permutation))))

(defmethod inverse-transform ((space strided-array-index-space)
                              &key scaling translation permutation)
  (make-instance
   'strided-array-index-space
   :ranges
   (let ((ranges (ranges space))
         (permutation (loop for i from 1 to (dimension space)
                            collect (1+ (position i permutation)))))
     (mapcar
      (lambda (scaling translation permutation)
        (let ((range (nth (1- permutation) ranges)))
          (range (/ (- (range-start range) translation) scaling)
                 (/ (range-step range) scaling)
                 (* (- (range-end range) translation) scaling))))
      scaling translation permutation))))

(defmethod reference ((object strided-array)
                      &key source-space target-space
                        scaling translation permutation)
  (let* ((args `(:scaling ,scaling
                 :translation ,translation
                 :permutation ,permutation))
         (source-space
           (or source-space
               (and target-space
                    (apply #'inverse-transform target-space args))
                           (index-space object)))
         (target-space
           (or target-space
               (apply #'transform source-space args))))
    (assert (equalp target-space (apply #'transform source-space args)))
    (make-instance
     'strided-array-reference
     :object object
     :ranges (ranges target-space))))
