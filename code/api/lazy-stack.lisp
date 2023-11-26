;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-stack (arrays &key (axis 0) (start 0) (step 1))
  (when (null arrays)
    (error "~@<Cannot stack zero arrays.~:@>"))
  ;; Stack the arrays.
  (apply
   #'lazy-fuse
   (let ((position start))
     (mapcar
      (lambda (lazy-array)
        (let* ((shape (lazy-array-shape lazy-array))
               (rank (shape-rank shape))
               (old-range
                 (if (< axis rank)
                     (shape-range shape axis)
                     (range 1)))
               (size (range-size old-range))
               (end (+ position (* size step)))
               (new-range (range position end step)))
          (setf position end)
          (lazy-reshape lazy-array
           (make-shape
            (if (zerop axis)
                (list new-range)
                (append
                 (subseq (shape-ranges shape) 0 (min rank axis))
                 (make-list (- axis (min rank axis)) :initial-element (range 1))
                 (list new-range)))))))
      (mapcar #'lazy-array arrays)))))
