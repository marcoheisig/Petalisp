;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun broadcast-arrays (array &rest more-arrays)
  (let ((n (1+ (length more-arrays))))
    (case n
      (1 (let ((lazy-array (coerce-to-lazy-array array)))
           (values (list lazy-array) (shape lazy-array))))
      (otherwise
       (let* ((arrays (list* array more-arrays))
              (lazy-arrays (mapcar #'coerce-to-lazy-array arrays))
              (shapes (mapcar #'shape lazy-arrays))
              (rank (loop for shape in shapes maximize (rank shape)))
              (modifiedp (loop for shape in shapes collect (/= rank (rank shape))))
              (broadcast-ranges '()))
         (loop for axis from (1- rank) downto 0 do
           (let ((broadcast-range nil))
             (loop for shape in shapes for modifiedp-cell on modifiedp do
               (let ((other-range (nth-broadcast-range shape rank axis)))
                 (if (null broadcast-range)
                     (setf broadcast-range other-range)
                     (setf broadcast-range
                           (broadcast-ranges broadcast-range other-range modifiedp-cell)))))
             (loop for shape in shapes for modifiedp-cell on modifiedp do
               (when (and (not (car modifiedp-cell))
                          (size-one-range-p (nth-broadcast-range shape rank axis))
                          (not (size-one-range-p broadcast-range)))
                 (setf (car modifiedp-cell) t)))
             (push broadcast-range broadcast-ranges)))
         (if (loop for elt in modifiedp never modifiedp)
             (values lazy-arrays (shape (first lazy-arrays)))
             (let ((broadcast-shape (make-shape broadcast-ranges)))
               (values
                (loop for lazy-array in lazy-arrays
                      for modified in modifiedp
                      collect
                      (if (not modified)
                          lazy-array
                          (reshape lazy-array broadcast-shape)))
                broadcast-shape))))))))

;;; Pad SHAPE with leading one element ranges until it reaches
;;; BROADCAST-RANK.  Then, access the range corresponding to AXIS of the
;;; resulting padded shape.
(defun nth-broadcast-range (shape broadcast-rank axis)
  (with-accessors ((rank rank) (ranges ranges)) shape
    (let* ((padding (- broadcast-rank rank))
           (n (- axis padding)))
      (if (minusp n)
          (range 0)
          (nth n ranges)))))

(defun broadcast-ranges (range-1 range-2 shape-2-modifiedp-cell)
  (declare (cons shape-2-modifiedp-cell))
  (symbol-macrolet ((shape-2-modifiedp (car shape-2-modifiedp-cell)))
    (if (size-one-range-p range-2)
        (prog1 range-1
          (when (and (not shape-2-modifiedp)
                     (not (set-equal range-1 range-2)))
            (setf shape-2-modifiedp t)))
        (if (or (size-one-range-p range-1)
                (set-equal range-1 range-2))
            range-2
            (error "~@<Cannot broadcast the ranges ~S and ~S.~:@>"
                   range-1 range-2)))))


(defun broadcasting-transformation (input-shape output-shape)
  (let* ((input-ranges (ranges input-shape))
         (output-ranges (ranges output-shape))
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
            (let ((output-size (set-size output-range))
                  (input-size (set-size input-range)))
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
