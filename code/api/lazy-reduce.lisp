;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun lazy-reduce (function &rest arrays &aux (n-values (length arrays)))
  (multiple-value-bind (inputs input-shape)
      (lazy-broadcast-list-of-arrays arrays)
    (unless (plusp (shape-rank input-shape))
      (error "Cannot reduce arrays with rank zero."))
    (unless (plusp (range-size (shape-range input-shape 0)))
      (error "Cannot reduce along an empty axis."))
    (lazy-reduce-aux function n-values inputs)))

(defun lazy-reduce-aux (function n-values inputs)
  (trivia:ematch (lazy-array-shape (first inputs))
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
                    (results
                      (multiple-value-list
                       (apply #'lazy-multiple-value function n-values
                              (petalisp.utilities:with-collectors ((arguments collect))
                                (dolist (input inputs)
                                  (collect (lazy-reshape input left)))
                                (dolist (input inputs)
                                  (collect (lazy-reshape input right)))
                                (arguments))))))
               (if (zerop rem)
                   (lazy-reduce-aux function n-values results)
                   (lazy-reduce-aux function n-values
                                    (let ((rest (~ last (1+ last) ~l more-ranges)))
                                      (loop for input in inputs
                                            for result in results
                                            collect
                                            (lazy-stack 0 result (lazy-reshape input rest)))))))))))))
