;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;; Make a transformation that maps every index of SHAPE to an element of
;;; the shape of ARRAY.
(defun make-broadcasting-transformation (array shape)
  (let* ((lazy-array (coerce-to-lazy-array array))
         (output-rank (rank lazy-array))
         (input-rank (shape-rank shape))
         (growth (- input-rank output-rank)))
    (unless (not (minusp growth))
      (error "~@<Cannot broadcast the rank ~D array ~S ~
                 to the rank ~D shape ~S.~:@>"
             output-rank array input-rank shape))
    (let ((input-mask (make-array input-rank :initial-element nil))
          (output-mask (make-array output-rank))
          (offsets (make-array output-rank))
          (scalings (make-array output-rank)))
      (loop for array-range of-type range in (shape-ranges (shape lazy-array))
            for shape-range of-type range in (nthcdr growth (shape-ranges shape))
            for oindex from 0
            for iindex from growth do
              (symbol-macrolet ((input-constraint (svref input-mask iindex))
                                (output-mask-entry (svref output-mask oindex))
                                (scaling (svref scalings oindex))
                                (offset (svref offsets oindex)))
                ;; Now we need to pick the appropriate input constraint,
                ;; output mask entry, scaling and offset to map from the
                ;; range of SHAPE to the corresponding range of ARRAY.
                (cond
                  ;; First, we pick of the trivial case of 'broadcasting' a
                  ;; one element range to another one element range.  This
                  ;; case is significant, because it allows us to introduce
                  ;; a suitable input constraint, thus increasing the
                  ;; chances that the resulting transformation is
                  ;; invertible.
                  ((size-one-range-p shape-range)
                   (unless (size-one-range-p array-range)
                     (error "~@<Cannot broadcast the range ~S
                              to the one element range ~S.~:@>"
                            shape-range array-range))
                   (setf input-constraint (range-start shape-range))
                   (setf output-mask-entry nil)
                   (setf offset (range-start array-range))
                   (setf scaling 0))
                  ;; The second case is that of collapsing an arbitrary
                  ;; range of SHAPE to a one element range of ARRAY.  It is
                  ;; similar to the previous case, except that we do not
                  ;; introduce an input constraint.
                  ((size-one-range-p array-range)
                   (setf output-mask-entry nil)
                   (setf offset (range-start array-range))
                   (setf scaling 0))
                  (t
                   ;; Now comes the general case.  We need to map an
                   ;; arbitrary range to another one.
                   (let* ((old-start (range-start shape-range))
                          (new-start (range-start array-range))
                          (old-step (range-step shape-range))
                          (new-step (range-step array-range))
                          (a (/ new-step old-step))
                          (b (- new-start (* a old-start))))
                     (setf output-mask-entry iindex)
                     (setf offset b)
                     (setf scaling a))))))
      (make-transformation
       :input-rank input-rank
       :output-rank output-rank
       :input-mask input-mask
       :output-mask output-mask
       :offsets offsets
       :scalings scalings))))

(defun broadcast (array shape)
  (let ((lazy-array (coerce-to-lazy-array array)))
    ;; Pick off the trivial case immediately.
    (if (shape-equal (shape lazy-array) shape)
        lazy-array
        (make-reference
         lazy-array
         shape
         (make-broadcasting-transformation array shape)))))

(defun broadcast-ranges (range-1 range-2)
  (cond ((size-one-range-p range-1) range-2)
        ((size-one-range-p range-2) range-1)
        ((= (range-size range-1)
            (range-size range-2))
         range-2)
        ((range-equal range-1 range-2) range-2)
        (t
         (error "~@<Cannot broadcast the ranges ~S and ~S.~:@>"
                range-1 range-2))))

(define-modify-macro broadcast-ranges-f (range-2)
  broadcast-ranges)

(defun broadcast-arrays (&rest arrays)
  (values-list
   (broadcast-list-of-arrays arrays)))

(defun broadcast-list-of-arrays (list-of-arrays)
  (let* ((lazy-arrays (mapcar #'coerce-to-lazy-array list-of-arrays))
         (shapes (mapcar #'shape lazy-arrays))
         (rank (loop for shape in shapes maximize (shape-rank shape)))
         (broadcast-ranges '()))
    (loop for axis from (1- rank) downto 0 do
      (let ((broadcast-range (range 0)))
        (loop for shape in shapes do
          (let ((other-range (nth-broadcast-range shape rank axis)))
            (broadcast-ranges-f broadcast-range other-range)))
        (push broadcast-range broadcast-ranges)))
    (let ((broadcast-shape (make-shape broadcast-ranges)))
      (values
       (loop for lazy-array in lazy-arrays
             for shape in shapes
             collect (broadcast lazy-array broadcast-shape))
       broadcast-shape))))

;;; Pad SHAPE with leading one element ranges until it reaches
;;; BROADCAST-RANK.  Then, access the range corresponding to AXIS of the
;;; resulting padded shape.
(defun nth-broadcast-range (shape broadcast-rank axis)
  (declare (shape shape))
  (let* ((padding (- broadcast-rank (shape-rank shape)))
         (n (- axis padding)))
    (if (minusp n)
        (range 0)
        (nth n (shape-ranges shape)))))

(defun broadcasting-transformation (input-shape output-shape)
  (let* ((input-ranges (shape-ranges input-shape))
         (output-ranges (shape-ranges output-shape))
         (input-rank (length input-ranges))
         (output-rank (length output-ranges))
         (offsets (make-array output-rank :initial-element 0))
         (scalings (make-array output-rank :initial-element 1))
         (input-mask
           (map 'simple-vector
                (lambda (range)
                  (when (size-one-range-p range)
                    (range-start range)))
                input-ranges)))
    (loop for index below (min input-rank output-rank)
          for input-range in input-ranges
          for output-range in output-ranges do
            (let ((output-size (range-size output-range))
                  (input-size (range-size input-range)))
              (cond ( ;; Select
                     (> output-size input-size)
                     (setf (svref offsets index) 0)
                     (setf (svref scalings index) 1))
                    ( ;; Move
                     (= output-size input-size)
                     (let ((scale (/ (range-step output-range)
                                     (range-step input-range))))
                       (setf (svref scalings index) scale)
                       (setf (svref offsets index)
                             (- (range-start output-range)
                                (* scale (range-start input-range))))))
                    ( ;; Broadcast
                     (= 1 output-size)
                     (setf (svref offsets index) (range-start output-range))
                     (setf (svref scalings index) 0))
                    (t (error "~@<Cannot broadcast the range ~S to the range ~S.~:@>"
                              input-range output-range)))))
    (make-transformation
     :input-rank input-rank
     :output-rank output-rank
     :offsets offsets
     :scalings scalings
     :input-mask input-mask)))
