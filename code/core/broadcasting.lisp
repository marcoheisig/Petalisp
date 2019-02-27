;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun broadcast-shapes (&rest objects)
  (let ((list-of-ranges (mapcar (alexandria:compose #'ranges #'shape) objects))
        (broadcast-ranges '()))
    (loop
      (let ((broadcast-range nil))
        (loop for cons on list-of-ranges do
          (unless (null (car cons))
            (let ((range (pop (car cons))))
              (cond ((null broadcast-range)
                     (setf broadcast-range range))
                    ((or (size-one-range-p range)
                         (set-equal range broadcast-range))
                     (values))
                    ((size-one-range-p broadcast-range)
                     (setf broadcast-range range))
                    (t
                     (error "~@<There is no common broadcast shape for the shapes ~
                                ~{~#[~;and ~S~;~S ~:;~S, ~]~}.~:@>"
                            (mapcar #'shape objects)))))))
        (if (null broadcast-range)
            (return (make-shape (nreverse broadcast-ranges)))
            (push broadcast-range broadcast-ranges))))))

#+nil
(defun broadcasting-transformation (shape)
  (make-transformation
   :output-rank 0
   :input-rank (rank shape)))

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
