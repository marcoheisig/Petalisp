;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defun β (function &rest arrays &aux (n-values (length arrays)))
  (multiple-value-bind (inputs input-shape)
      (broadcast-list-of-arrays arrays)
    (cond ((null input-shape)
           (empty-arrays n-values))
          ((zerop (shape-rank input-shape))
           (empty-arrays n-values))
          (t
           (β-aux function n-values inputs)))))

(defun β-aux (function n-values inputs)
  (trivia:ematch (array-shape (first inputs))
    ((~r range ~l more-ranges)
     (if (size-one-range-p range)
         (values-list
          (mapcar (lambda (input) (drop-axes input 0)) inputs))
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
                       (apply #'α* n-values function
                              (petalisp.utilities:with-collectors ((arguments collect))
                                (dolist (input inputs)
                                  (collect (reshape input left)))
                                (dolist (input inputs)
                                  (collect (reshape input right)))
                                (arguments))))))
               (if (zerop rem)
                   (β-aux function n-values values)
                   (β-aux function n-values
                          (let ((rest (~ last (1+ last) ~l more-ranges)))
                            (loop for input in inputs
                                  for value in values
                                  collect
                                  (stack 0 value (reshape input rest)))))))))))))

(defun β* (f z x &optional axis)
  (cond ((empty-array-p x) z)
        ((typep axis 'rank)
         (β f (move-axis-to-front x axis)))
        ((null axis)
         (loop repeat (rank x)
               do (setf x (β f x))
               finally (return x)))
        (t (error "Not a valid axis: ~S" axis))))
