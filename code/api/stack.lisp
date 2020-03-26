;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun stack (axis &rest arrays)
  (flet ((stacker (axis)
           (let ((offset 0))
             (lambda (array)
               (let* ((lazy-array (lazy-array array))
                      (shape (shape lazy-array))
                      (rank (shape-rank shape))
                      (offsets (make-array rank :initial-element 0)))
                 (unless (< -1 axis rank)
                   (error "~@<Invalid stack axis ~S for the array ~S.~:@>"
                          axis array))
                 (setf (aref offsets axis) offset)
                 (prog1 (reshape lazy-array (make-transformation :offsets offsets))
                   (incf offset
                         (let ((range (nth axis (shape-ranges shape))))
                           (- (+ (range-end range) (range-step range))
                              (range-start range))))))))))
    (apply #'fuse (mapcar (stacker axis) arrays))))
