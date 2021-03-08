;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-reshape (array &rest modifiers)
  (reduce
   (lambda (lazy-array modifier)
     (typecase modifier
       (transformation (transform-lazy-array lazy-array modifier))
       (shape (lazy-reshape-using-shape lazy-array modifier))
       (otherwise (lazy-reshape-using-shape lazy-array (array-shape modifier)))))
   modifiers
   :initial-value (lazy-array array)))

(defun lazy-reshape-using-shape (lazy-array shape)
  (let ((lazy-array-shape (lazy-array-shape lazy-array)))
    (if (and (= (shape-size lazy-array-shape)
                (shape-size shape)))
        ;; Case 1 - Reshaping while preserving the number of elements.
        (change-shape lazy-array shape)
        ;; Case 2 - Broadcasting or selection of a subspace.
        (multiple-value-bind (transformation broadcast-p select-p)
            (make-shape-transformation shape lazy-array-shape)
          ;; We do not allow transformations that broadcast some ranges, but
          ;; shrink others.  Otherwise, we would risk ambiguity with case 1.
          (unless (not (and broadcast-p select-p))
            (error "~@<Cannot reshape the array ~S ~
                 to the shape ~S.~:@>"
                   lazy-array shape))
          (lazy-ref lazy-array shape transformation)))))

;;; The function MAKE-SHAPE-TRANSFORMATION returns three values:
;;;
;;; 1. A transformation that maps each element of INPUT-SHAPE to an element
;;;    of OUTPUT-SHAPE.
;;;
;;; 2. A boolean, indicating whether broadcasting has been performed.
;;;
;;; 3. A boolean, indicating whether some values of OUTPUT-SHAPE have been
;;;    discarded.
;;;
;;; An error is signaled when there is no sensible way to transform
;;; INPUT-SHAPE to the supplied OUTPUT-SHAPE.
;;;
;;; This function is primarily used for broadcasting.  In that case the
;;; INPUT shape is the desired shape after broadcasting, and the
;;; OUTPUT-SHAPE is that of the array to be broadcast.
(defun make-shape-transformation (input-shape output-shape)
  (if (shape-equal output-shape input-shape)
      (identity-transformation (shape-rank input-shape))
      (let* ((output-rank (shape-rank output-shape))
             (input-rank (shape-rank input-shape))
             (growth (- input-rank output-rank)))
        (unless (not (minusp growth))
          (error "~@<Cannot reshape the rank ~D shape ~S ~
                     to the rank ~D shape ~S.~:@>"
                 output-rank output-shape input-rank input-shape))
        (let ((input-mask (make-array input-rank :initial-element nil))
              (output-mask (make-array output-rank))
              (offsets (make-array output-rank))
              (scalings (make-array output-rank))
              (select-p nil))
          (loop for output-range in (shape-ranges output-shape)
                for input-range in (nthcdr growth (shape-ranges input-shape))
                for oindex from 0
                for iindex from growth do
                  (symbol-macrolet ((input-constraint (svref input-mask iindex))
                                    (output-mask-entry (svref output-mask oindex))
                                    (scaling (svref scalings oindex))
                                    (offset (svref offsets oindex)))
                    ;; Now we need to pick the appropriate input
                    ;; constraint, output mask entry, scaling and offset to
                    ;; map from the range of INPUT-SHAPE to the
                    ;; corresponding range of OUTPUT-SHAPE.
                    (cond
                      ;; First, we pick of the trivial case of reshaping a one
                      ;; element range to another one element range.  This case
                      ;; is significant, because it allows us to introduce a
                      ;; suitable input constraint, thus increasing the chances
                      ;; that the resulting transformation is invertible.
                      ((and (size-one-range-p input-range)
                            (size-one-range-p output-range))
                       (setf input-constraint (range-start input-range))
                       (setf output-mask-entry nil)
                       (setf offset (range-start output-range))
                       (setf scaling 0))
                      ;; The second case is that of collapsing an arbitrary
                      ;; range of INPUT-SHAPE to a one element range of
                      ;; OUTPUT-SHAPE, i.e., actual broadcasting.  It is
                      ;; similar to the previous case, except that we do
                      ;; not introduce an input constraint.
                      ((size-one-range-p output-range)
                       (setf output-mask-entry nil)
                       (setf offset (range-start output-range))
                       (setf scaling 0))
                      ;; The third case is that of mapping an arbitrary range
                      ;; to another one of the same size.
                      ((= (range-size input-range)
                          (range-size output-range))
                       (let* ((old-start (range-start input-range))
                              (new-start (range-start output-range))
                              (old-step (range-step input-range))
                              (new-step (range-step output-range))
                              (a (/ new-step old-step))
                              (b (- new-start (* a old-start))))
                         (setf output-mask-entry iindex)
                         (setf offset b)
                         (setf scaling a)))
                      ;; The fourth case is that of selecting a subset of the
                      ;; elements of OUTPUT-RANGE.
                      ((subrangep input-range output-range)
                       (setf output-mask-entry iindex)
                       (setf offset 0)
                       (setf scaling 1)
                       (setf select-p t))
                      ;; Otherwise, an error is signaled.
                      (t
                       (error "~@<Cannot reshape the range ~S ~
                                  to the range ~S.~:@>"
                              input-range output-range)))))
          (let ((transformation
                  (make-transformation
                   :input-rank input-rank
                   :output-rank output-rank
                   :input-mask input-mask
                   :output-mask output-mask
                   :offsets offsets
                   :scalings scalings)))
            (values
             transformation
             (not (transformation-invertiblep transformation))
             select-p))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Size-Preserving Reshape

(defun change-shape (lazy-array shape)
  (let ((n1 (normalizing-transformation (array-shape lazy-array)))
        (n2 (normalizing-transformation shape)))
    (lazy-reshape
     (change-shape/normalized
      (lazy-reshape lazy-array n1)
      (transform-shape shape n2))
     (invert-transformation n2))))

(defun change-shape/normalized (lazy-array output-shape)
  ;; As a small, but important optimization, we detect whether a prefix or
  ;; suffix of the ranges of LAZY-ARRAY matches a prefix or suffix of the
  ;; ranges of SHAPE.  If so, they are left unchanged for the entire
  ;; procedure.
  (let* ((input-shape (array-shape lazy-array))
         (input-rank (shape-rank input-shape))
         (output-rank (shape-rank output-shape))
         (input-ranges (shape-ranges input-shape))
         (output-ranges (shape-ranges output-shape))
         (start (mismatch input-ranges output-ranges :key #'range-size :from-end nil)))
    (if (not start) ; If there is no mismatch, we don't have to do anything.
        lazy-array
        (let ((end (mismatch input-ranges output-ranges :key #'range-size :from-end t)))
          (reshape/unflatten
           (reshape/flatten lazy-array start end)
           output-shape
           start
           (+ end (- output-rank input-rank)))))))

(defun reshape/flatten (lazy-array start end)
  (multiple-value-bind (vector-of-prime-factors pivot)
      (factorize-shape (array-shape lazy-array) start end)
    ;; Flatten all ranges above PIVOT.
    (loop for index from (1+ pivot) below end
          for prime-factors = (aref vector-of-prime-factors (- index start)) do
            (loop for prime-factor in (rest prime-factors) do
              (setf lazy-array (insert-axis-before lazy-array (1+ pivot) prime-factor))
              (setf lazy-array (remove-axis-after lazy-array pivot)))
            (setf lazy-array (remove-axis-after lazy-array pivot)))
    ;; Flatten all ranges below PIVOT.
    (loop for index from (1- pivot) downto start
          for prime-factors = (aref vector-of-prime-factors (- index start)) do
            (loop for prime-factor in (rest prime-factors) do
              (setf lazy-array (insert-axis-after lazy-array index prime-factor))
              (setf lazy-array (remove-axis-before lazy-array (1+ index))))
            (setf lazy-array (remove-axis-before lazy-array (1+ index))))
    lazy-array))

(defun reshape/unflatten (lazy-array shape start end)
  (multiple-value-bind (vector-of-prime-factors pivot)
      (factorize-shape shape start end)
    ;; Unflatten all ranges above PIVOT.
    (loop for index from (1- end) above pivot
          for prime-factors = (aref vector-of-prime-factors (- index start)) do
            (setf lazy-array (insert-axis-after lazy-array start (first prime-factors)))
            (loop for prime-factor in (rest prime-factors) do
              (setf lazy-array (insert-axis-after lazy-array start prime-factor))
              (setf lazy-array (remove-axis-before lazy-array (+ start 2)))))
    ;; Unflatten all ranges below PIVOT.
    (loop for index from start below pivot
          for prime-factors = (aref vector-of-prime-factors (- index start)) do
            (setf lazy-array (insert-axis-before lazy-array index (first prime-factors)))
            (loop for prime-factor in (rest prime-factors) do
              (setf lazy-array (insert-axis-before lazy-array (1+ index) prime-factor))
              (setf lazy-array (remove-axis-after lazy-array index))))
    (lazy-ref
     lazy-array
     shape
     (make-shape-transformation shape (array-shape lazy-array)))))

;;; Return a vector with the prime factors of SHAPE bounded by START and
;;; END, and the axis with the larges sum of prime factors therein.
(defun factorize-shape (shape start end)
  (let* ((size (- end start))
         (vector-of-prime-factors (make-array size))
         (maximum 1)
         (position start))
    (loop for range in (nthcdr start (shape-ranges shape))
          for index from 0 below size do
            (let* ((prime-factors (petalisp.utilities:prime-factors (range-size range)))
                   (sum (reduce #'+ prime-factors)))
              (setf (aref vector-of-prime-factors index) prime-factors)
              (when (>= sum maximum)
                (setf maximum sum)
                (setf position index))))
    (values
     vector-of-prime-factors
     (+ start position))))

;; Turn the range at the supplied AXIS with size N into a range of size K,
;; followed by a range of size N / K.
(defun insert-axis-before (lazy-array axis k)
  (let* ((shape (array-shape lazy-array))
         (rank (rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 axis))
         (range (nth axis ranges))
         (suffix (subseq ranges (1+ axis)))
         (range-2 (range 0 (/ (range-size range) k))))
    (apply
     #'lazy-fuse
     (loop for offset below k
           collect
           (lazy-ref
            lazy-array
            (~l prefix ~ offset (1+ offset) ~r range-2 ~l suffix)
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
  (let* ((shape (array-shape lazy-array))
         (rank (rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 axis))
         (suffix (subseq ranges (1+ axis)))
         (n (range-size (nth axis ranges)))
         (range-1 (range 0 (/ n k))))
    (apply
     #'lazy-fuse
     (loop for offset below k
           collect
           (lazy-ref
            lazy-array
            (~l prefix ~r range-1 ~ offset (1+ offset) ~l suffix)
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
  (let* ((shape (array-shape lazy-array))
         (rank (shape-rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 (1- axis)))
         (suffix (subseq ranges (1+ axis)))
         (range-1 (nth (1- axis) ranges))
         (range-2 (nth axis ranges))
         (size-1 (range-size range-1))
         (size-2 (range-size range-2)))
    (apply
     #'lazy-fuse
     (loop for offset below size-1
           collect
           (lazy-ref
            lazy-array
            (~l prefix ~ (* offset size-2) (* (1+ offset) size-2) ~l suffix)
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
  (let* ((shape (array-shape lazy-array))
         (rank (shape-rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 axis))
         (suffix (subseq ranges (+ axis 2)))
         (range-1 (nth axis ranges))
         (range-2 (nth (1+ axis) ranges))
         (size-1 (range-size range-1))
         (size-2 (range-size range-2)))
    (apply
     #'lazy-fuse
     (loop for offset below size-2
           collect
           (lazy-ref
            lazy-array
            (~l prefix ~ offset (* size-1 size-2) size-2 ~l suffix)
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
