;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-reduce (function &rest arrays &aux (n-values (length arrays)))
  (multiple-value-bind (inputs input-shape)
      (lazy-broadcast-list-of-arrays arrays)
    (cond ((empty-shape-p input-shape)
           (empty-arrays n-values))
          ((zerop (shape-rank input-shape))
           (empty-arrays n-values))
          (t
           (lazy-reduce-aux function n-values inputs)))))

(defun lazy-reduce-aux (function n-values inputs)
  (trivia:ematch (array-shape (first inputs))
    ((~r range ~l more-ranges)
     (if (size-one-range-p range)
         (values-list
          (mapcar (lambda (input) (lazy-drop-axes input 0)) inputs))
         (multiple-value-bind (n rem) (floor (range-size range) 2)
           (with-accessors ((start range-start)
                            (last range-last)
                            (step range-step)) range
             (let* ((left (~ start
                             (+ step start (* 2 step (1- n)))
                             (* 2 step)
                             ~l more-ranges))
                    (right (~ (+ start step)
                              (+ step start (* 2 step n))
                              (* 2 step)
                              ~l more-ranges))
                    (values
                      (multiple-value-list
                       (apply #'lazy-multiple-value function n-values
                              (petalisp.utilities:with-collectors ((arguments collect))
                                (dolist (input inputs)
                                  (collect (lazy-reshape input left)))
                                (dolist (input inputs)
                                  (collect (lazy-reshape input right)))
                                (arguments))))))
               (if (zerop rem)
                   (lazy-reduce-aux function n-values values)
                   (lazy-reduce-aux function n-values
                          (let ((rest (~ last (1+ last) ~l more-ranges)))
                            (loop for input in inputs
                                  for value in values
                                  collect
                                  (lazy-stack 0 value (lazy-reshape input rest)))))))))))))
