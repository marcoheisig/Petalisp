;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-reduce (function &rest arrays)
  (multiple-value-bind (inputs input-shape)
      (lazy-broadcast-list-of-arrays arrays)
    (unless (plusp (shape-rank input-shape))
      (error "Cannot reduce arrays with rank zero."))
    (unless (plusp (range-size (shape-range input-shape 0)))
      (error "Cannot reduce along an empty axis."))
    (values-list
     (lazy-reduce-aux function (length inputs) inputs))))

(defun lazy-reduce-aux (function n-values inputs)
  (let ((range (shape-range (lazy-array-shape (first inputs)) 0)))
    (if (size-one-range-p range)
        (mapcar (lambda (input) (lazy-drop-axes input 0)) inputs)
        (with-accessors ((start range-start)
                         (end range-end)
                         (last range-last)
                         (step range-step)
                         (size range-size)) range
          (let* ((rem (rem (range-size range) 2))
                 (left  (~    start       (- end (* step rem)) (* 2 step)))
                 (right (~ (+ start step) (- end (* step rem)) (* 2 step)))
                 (rest  (if (zerop rem) (~ 0) (~ last (1+ last))))
                 (results
                   (multiple-value-list
                    (apply #'lazy-multiple-value function n-values
                           (petalisp.utilities:with-collectors ((arguments collect))
                             (dolist (input inputs)
                               (collect (lazy-reshape input 1 left (collapsing-reshaper))))
                             (dolist (input inputs)
                               (collect (lazy-reshape input 1 right (collapsing-reshaper))))
                             (arguments))))))
            (lazy-reduce-aux
             function
             n-values
             (loop for input in inputs
                   for result in results
                   collect
                   (lazy-stack 0 result (lazy-reshape input 1 rest)))))))))
