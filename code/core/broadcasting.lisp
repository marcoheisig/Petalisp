;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun broadcast (array shape)
  (let* ((lazy-array (coerce-to-lazy-array array))
         (old-rank (rank lazy-array))
         (new-rank (shape-rank shape)))
    (cond ((shape-equal (shape array) shape) lazy-array)
          ((< new-rank old-rank)
           (error "~@<Cannot broadcast the rank ~D array ~A ~
                      to the rank ~D shape ~A.~:@>"
                  old-rank array new-rank shape))
          (t
           (let ((output-mask (make-array old-rank))
                 (offsets (make-array old-rank))
                 (scalings (make-array old-rank)))
             (loop for range in (shape-ranges (shape lazy-array))
                   for oindex from 0
                   for iindex from (- new-rank old-rank) do
                     (if (size-one-range-p range)
                         (setf (svref output-mask oindex) nil
                               (svref offsets oindex) (range-start range)
                               (svref scalings oindex) 0)
                         (setf (svref output-mask oindex) iindex
                               (svref offsets oindex) 0
                               (svref scalings oindex) 1)))
             (make-reference
              lazy-array
              shape
              (make-transformation
               :input-rank new-rank
               :output-rank old-rank
               :output-mask output-mask
               :offsets offsets
               :scalings scalings)))))))

(defun broadcast-ranges (range-1 range-2)
  (cond ((size-one-range-p range-1) range-2)
        ((size-one-range-p range-2) range-1)
        ((range-equal range-1 range-2) range-2)
        (t
         (error "~@<Cannot broadcast the ranges ~S and ~S.~:@>"
                range-1 range-2))))

(define-modify-macro broadcast-ranges-f (range-2)
  broadcast-ranges)

(defun broadcast-arrays (&rest arrays)
  (let ((n (length arrays)))
    (case n
      (0 (values))
      (1 (values (coerce-to-lazy-array (first arrays))))
      (otherwise
       (values-list
        (broadcast-list-of-arrays arrays))))))

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
                    (t (error "Cannot broadcast the range ~S to the range ~S."
                              input-range output-range)))))
    (make-transformation
     :input-rank input-rank
     :output-rank output-rank
     :offsets offsets
     :scalings scalings
     :input-mask input-mask)))
