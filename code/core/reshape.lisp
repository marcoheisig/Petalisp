;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun reshape (array &rest modifiers)
  (reduce (lambda (lazy-array modifier)
            (if (transformationp modifier)
                (reshape-using-transformation lazy-array modifier)
                (reshape-using-shape lazy-array (shape modifier))))
          modifiers
          :initial-value (coerce-to-lazy-array array)))

(defun reshape-using-transformation (lazy-array transformation)
  (make-reference
   lazy-array
   (transform (shape lazy-array) transformation)
   (invert-transformation transformation)))

(defun reshape-using-shape (lazy-array shape)
  (let ((array-shape (shape lazy-array)))
    (if (and (= (shape-size array-shape)
                (shape-size shape)))
        ;; Case 1 - Reshaping while preserving the number of elements.
        (change-shape lazy-array shape)
        ;; Case 2 - Broadcasting or selection of a subspace.
        (multiple-value-bind (transformation broadcast-p select-p)
            (make-shape-transformation shape array-shape)
          ;; We do not allow transformations that broadcast some ranges, but
          ;; shrink others.  Otherwise, we would risk ambiguity with case 1.
          (unless (not (and broadcast-p select-p))
            (error "~@<Cannot reshape the array ~S ~
                 to the shape ~S.~:@>"
                   lazy-array shape))
          (make-reference lazy-array shape transformation)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Size-Preserving Reshape
;;;
;;; The central data structures of CHANGE-SHAPE are two vectors of prime
;;; factors - simple vectors whose elements are the lists of prime factors
;;; of the ranges of the supplied lazy array and shape.  The goal is to
;;; reorder the prime factors of LAZY-ARRAY until they match those of
;;; SHAPE.  To do so, we augment the former vector with two additional,
;;; empty elements at the beginning and the end.
;;;
;;; To simplify the implementation, the lazy array is immediately
;;; transformed such that all its ranges start from zero, and such that
;;; there are not ranges of size one involved.
;;;
;;; These mechanisms are best viewed on an example.  Imagine the task is to
;;; reshape an array with shape (~ 1 10 ~ 1 2 ~ 1 3) to the equally large
;;; shape (~ 0 3 ~ 0 4 ~ 0 2).  In this case, the vector of prime factors
;;; of the target shape is #((2 2) (5) (3)), and the augmented vector of
;;; prime factors of the supplied lazy array is #(nil (2 5) (2) (3) nil).
;;;
;;; #(() (2 5) (2) (3) ()) ; Initial configuration
;;;   ^                ^
;;;
;;; #(() (2 5) (2) () (3)) ; Skip right
;;;   ^            ^
;;;
;;; #((2) (5) (2) () (3)) ; Grow left
;;;   ^   °       ^
;;;
;;; #((2) (2 5) () () (3)) ; Gather left+1
;;;   ^   °         ^
;;;
;;; #((2 2) (5) () () (3)) ; Grow left
;;;             ^  ^

(defun change-shape (lazy-array shape)
  (let ((n1 (normalizing-transformation (shape lazy-array)))
        (n2 (normalizing-transformation shape)))
    (reshape (unflatten (flatten (reshape lazy-array n1))
                        (transform shape n2))
             (invert-transformation n2))))

(defun flatten (lazy-array)
  (if (zerop (rank lazy-array))
      (make-reference lazy-array (~ 0) (τ (0) nil))
      (multiple-value-bind (vector-of-prime-factors pivot)
          (factorize-shape (shape lazy-array))
        ;; Flatten all ranges above PIVOT.
        (loop for index from (1+ pivot) below (length vector-of-prime-factors)
              for prime-factors = (aref vector-of-prime-factors index) do
                (loop for prime-factor in (rest prime-factors) do
                  (setf lazy-array (insert-axis-before lazy-array (1+ pivot) prime-factor))
                  (setf lazy-array (remove-axis-after lazy-array pivot)))
                (setf lazy-array (remove-axis-after lazy-array pivot)))
        ;; Flatten all ranges below PIVOT.
        (loop for index from (1- pivot) downto 0
              for prime-factors = (aref vector-of-prime-factors index) do
                (loop for prime-factor in (rest prime-factors) do
                  (setf lazy-array (insert-axis-after lazy-array index prime-factor))
                  (setf lazy-array (remove-axis-before lazy-array (1+ index))))
                (setf lazy-array (remove-axis-before lazy-array (1+ index))))
        lazy-array)))

(defun unflatten (lazy-array shape)
  (if (zerop (rank shape))
      (reshape lazy-array (τ (0) ()))
      (multiple-value-bind (vector-of-prime-factors pivot)
          (factorize-shape shape)
        ;; Unflatten all ranges above PIVOT.
        (loop for index from (1- (length vector-of-prime-factors)) above pivot
              for prime-factors = (aref vector-of-prime-factors index) do
                (setf lazy-array (insert-axis-after lazy-array 0 (first prime-factors)))
                (loop for prime-factor in (rest prime-factors) do
                  (setf lazy-array (insert-axis-after lazy-array 0 prime-factor))
                  (setf lazy-array (remove-axis-before lazy-array 2))))
        ;; Unflatten all ranges below PIVOT.
        (loop for index from 0 below pivot
              for prime-factors = (aref vector-of-prime-factors index) do
                (setf lazy-array (insert-axis-before lazy-array index (first prime-factors)))
                (loop for prime-factor in (rest prime-factors) do
                  (setf lazy-array (insert-axis-before lazy-array (1+ index) prime-factor))
                  (setf lazy-array (remove-axis-after lazy-array index))))
        (make-reference
         lazy-array
         shape
         (make-shape-transformation shape (shape lazy-array))))))

(defun factorize-shape (shape)
  (let ((vector-of-prime-factors (make-array (rank shape)))
        (most-positive-prime-factor 1)
        (most-positive-prime-factor-index 0))
    (loop for range in (shape-ranges shape)
          for index from 0 do
            (let* ((prime-factors (petalisp.utilities:prime-factors (range-size range)))
                   (max (apply #'max prime-factors)))
              (setf (aref vector-of-prime-factors index) prime-factors)
              (when (>= max most-positive-prime-factor)
                (setf most-positive-prime-factor max)
                (setf most-positive-prime-factor-index index))))
    (values vector-of-prime-factors most-positive-prime-factor-index)))

;; Turn the range at the supplied AXIS with size N into a range of size K,
;; followed by a range of size N / K.
(defun insert-axis-before (lazy-array axis k)
  (let* ((shape (shape lazy-array))
         (rank (rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 axis))
         (range (nth axis ranges))
         (suffix (subseq ranges (1+ axis)))
         (range-2 (range 0 (1- (/ (range-size range) k)))))
    (apply #'fuse
           (loop for offset below k
                 collect
                 (make-reference
                  lazy-array
                  (make-shape
                   (append prefix (list (range offset) range-2) suffix))
                  (let ((input-mask (make-array (1+ rank) :initial-element nil))
                        (output-mask (make-array rank :initial-element nil))
                        (offsets (make-array rank :initial-element 0)))
                    (loop for index below axis do
                      (setf (aref output-mask index) index))
                    (setf (aref input-mask axis) offset)
                    (setf (aref output-mask axis) (1+ axis))
                    (setf (aref offsets axis) (* offset (range-size range-2)))
                    (loop for index from (1+ axis) below rank do
                      (setf (aref output-mask index) (1+ index)))
                    (make-transformation
                     :input-mask input-mask
                     :output-mask output-mask
                     :offsets offsets)))))))

;; Turn the range at axis I with size N into a range of size N / K,
;; followed by a range of size K.
(defun insert-axis-after (lazy-array axis k)
  (let* ((shape (shape lazy-array))
         (rank (rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 axis))
         (suffix (subseq ranges (1+ axis)))
         (n (range-size (nth axis ranges)))
         (range-1 (range 0 (1- (/ n k)))))
    (apply #'fuse
           (loop for offset below k
                 collect
                 (make-reference
                  lazy-array
                  (make-shape
                   (append prefix (list range-1 (range offset)) suffix))
                  (let ((input-mask (make-array (1+ rank) :initial-element nil))
                        (output-mask (make-array rank :initial-element nil))
                        (offsets (make-array rank :initial-element 0))
                        (scalings (make-array rank :initial-element 1)))
                    (loop for index below axis do
                      (setf (aref output-mask index) index))
                    (setf (aref output-mask axis) axis)
                    (setf (aref offsets axis) offset)
                    (setf (aref scalings axis) k)
                    (setf (aref input-mask (1+ axis)) offset)
                    (loop for index from (1+ axis) below rank do
                      (setf (aref output-mask index) (1+ index)))
                    (make-transformation
                     :input-mask input-mask
                     :output-mask output-mask
                     :offsets offsets
                     :scalings scalings)))))))

(defun remove-axis-before (lazy-array axis)
  (let* ((shape (shape lazy-array))
         (rank (shape-rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 (1- axis)))
         (suffix (subseq ranges (1+ axis)))
         (range-1 (nth (1- axis) ranges))
         (range-2 (nth axis ranges))
         (size-1 (range-size range-1))
         (size-2 (range-size range-2)))
    (apply #'fuse
           (loop for offset below size-1
                 collect
                 (make-reference
                  lazy-array
                  (make-shape
                   (append prefix (list (range (* offset size-2) (1- (* (1+ offset) size-2)))) suffix))
                  (let ((input-mask (make-array (1- rank) :initial-element nil))
                        (output-mask (make-array rank :initial-element nil))
                        (offsets (make-array rank :initial-element 0)))
                    (loop for index below (1- axis) do
                      (setf (aref output-mask index) index))
                    (setf (aref output-mask (1- axis)) nil)
                    (setf (aref offsets (1- axis)) offset)
                    (setf (aref output-mask axis) (1- axis))
                    (setf (aref offsets axis) (- (* offset size-2)))
                    (loop for index from (1+ axis) below rank do
                      (setf (aref output-mask index) (1- index)))
                    (make-transformation
                     :input-mask input-mask
                     :output-mask output-mask
                     :offsets offsets)))))))

(defun remove-axis-after (lazy-array axis)
  (let* ((shape (shape lazy-array))
         (rank (shape-rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 axis))
         (suffix (subseq ranges (+ axis 2)))
         (range-1 (nth axis ranges))
         (range-2 (nth (1+ axis) ranges))
         (size-1 (range-size range-1))
         (size-2 (range-size range-2)))
    (apply #'fuse
           (loop for offset below size-2
                 collect
                 (make-reference
                  lazy-array
                  (make-shape
                   (append prefix (list (range offset size-2 (1- (* size-1 size-2)))) suffix))
                  (let ((input-mask (make-array (1- rank) :initial-element nil))
                        (output-mask (make-array rank :initial-element nil))
                        (scalings (make-array rank :initial-element 1))
                        (offsets (make-array rank :initial-element 0)))
                    (loop for index below axis do
                      (setf (aref output-mask index) index))
                    (setf (aref output-mask axis) axis)
                    (setf (aref offsets axis) (- (/ offset size-2)))
                    (setf (aref scalings axis) (/ size-2))
                    (setf (aref output-mask (1+ axis)) nil)
                    (setf (aref offsets (1+ axis)) offset)
                    (loop for index from (+ axis 2) below rank do
                      (setf (aref output-mask index) (1- index)))
                    (make-transformation
                     :input-mask input-mask
                     :output-mask output-mask
                     :offsets offsets
                     :scalings scalings)))))))
