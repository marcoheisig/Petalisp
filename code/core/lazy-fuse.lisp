;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun fuse (&rest inputs)
  (make-fusion (mapcar #'lazy-array inputs)))

(defun fuse* (&rest inputs)
  (let ((lazy-arrays (mapcar #'lazy-array inputs)))
    (unless (petalisp.utilities:identical lazy-arrays :test #'= :key #'rank)
      (error "~@<Can only fuse arrays with equal rank.  The arrays ~
                 ~{~#[~;and ~S~;~S ~:;~S, ~]~} violate this ~
                 requirement.~:@>"
             inputs)))
  (let* ((lazy-arrays (mapcar #'lazy-array inputs))
         (identity (identity-transformation (rank (first lazy-arrays)))))
    (make-fusion
     (mapcar
      (lambda (fragment)
        (destructuring-bind (shape . bitmask) fragment
          (lazy-reference (nth (1- (integer-length bitmask)) lazy-arrays) shape identity)))
      (subdivide lazy-arrays)))))

;; Create a fusion, assuming INPUTS are non-empty, non-overlapping lazy-arrays.
(defun make-fusion (inputs)
  (let ((shape (fuse-shapes (mapcar #'shape inputs))))
    (trivia:match inputs
      ((list) (empty-array))
      ((list x) x)
      (_ (make-instance 'lazy-fuse
           :ntype (reduce #'petalisp.type-inference:ntype-union
                          inputs
                          :key #'element-ntype)
           :inputs inputs
           :shape shape)))))

(defun fuse-shapes (shapes)
  (trivia:match shapes
    ((list) nil)
    ((list shape) shape)
    (_
     ;; Ensure that all shapes have the same rank.
     (let* ((first-shape (first shapes))
            (rank (shape-rank first-shape)))
       (loop for shape in (rest shapes) do
         (unless (= (shape-rank shape) rank)
           (error "~@<Can only fuse shapes with ~
                       equal rank. The shapes ~{~#[~;and ~S~;~S ~:;~S, ~]~} ~
                       violate this requirement.~:@>"
                  shapes)))
       ;; Ensure that all shapes are disjoint.
       (alexandria:map-combinations
        (lambda (two-shapes)
          (destructuring-bind (shape-1 shape-2) two-shapes
            (when (shape-intersectionp shape-1 shape-2)
              (error "~@<Can only fuse disjoint shapes, ~
                         but the shape ~S and the shape ~S have the ~
                         common subshape ~S.~:@>"
                     shape-1
                     shape-2
                     (shape-intersection shape-1 shape-2)))))
        shapes :length 2 :copy nil)
       ;; Guess the only plausible union shape.
       (let ((union (%make-shape
                     (apply #'mapcar #'fuse-shapes/range-oracle
                            (mapcar #'shape-ranges shapes))
                     rank)))
         ;; The union shape is only valid if it has exactly as many
         ;; elements as all the inputs combined.
         (unless (= (reduce #'+ shapes :key #'shape-size)
                    (shape-size union))
           (error "~@<Cannot fuse the shapes ~
                      ~{~#[~;and ~S~;~S ~:;~S, ~]~}.~:@>"
                  shapes))
         union)))))

;;; Determine the 'bounding box' of the supplied RANGES.
(defun fuse-shapes/range-oracle (&rest ranges)
  (loop for range in ranges
        minimize (range-start range) into global-start
        maximize (range-end range) into global-end
        finally
           (return
             (if (= global-start global-end)
                 (first ranges)
                 ;; now determine the step size
                 (let ((step-size (- global-end global-start)))
                   (dolist (range ranges)
                     (flet ((probe (n)
                              (setf step-size
                                    (min step-size
                                         (- n global-start)))))
                       (if (> (range-start range) global-start)
                           (probe (range-start range))
                           (unless (size-one-range-p range)
                             (probe (+ (range-start range)
                                       (range-step range)))))))
                   (make-range global-start step-size global-end))))))
