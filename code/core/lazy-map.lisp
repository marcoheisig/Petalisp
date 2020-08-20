;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun single-value-lazy-map (shape function inputs)
  (if (empty-shape-p shape)
      (empty-array)
      (petalisp.type-inference:specialize
       function
       inputs
       #'element-ntype
       (lambda (constant)
         (lazy-reshape
          (make-scalar-immediate constant)
          shape
          (make-transformation
           :input-rank (shape-rank shape)
           :output-rank 0)))
       (lambda (ntypes function inputs)
         (make-instance 'single-value-lazy-map
           :operator function
           :inputs inputs
           :shape shape
           :ntype (first ntypes)))
       (lambda ()
         (make-instance 'single-value-lazy-map
           :operator function
           :inputs inputs
           :shape shape
           :ntype (petalisp.type-inference:ntype 't))))))

(defun multiple-value-lazy-map (n-outputs shape function inputs)
  (if (empty-shape-p shape)
      (empty-arrays n-outputs)
      (let ((identity (cons nil nil)))
        (petalisp.type-inference:specialize
         function
         inputs
         #'element-ntype
         (lambda (constant)
           (lazy-reshape
            (make-scalar-immediate constant)
            shape
            (make-transformation
             :input-rank (shape-rank shape)
             :output-rank 0)))
         (lambda (ntypes function inputs)
           (values-list
            (loop for ntype in ntypes
                  for value-n from 0
                  collect
                  (make-instance 'multiple-value-lazy-map
                    :operator function
                    :value-n value-n
                    :inputs inputs
                    :shape shape
                    :identity identity
                    :number-of-values n-outputs
                    :ntype ntype))))
         (lambda ()
           (values-list
            (loop for value-n below n-outputs
                  collect
                  (make-instance 'multiple-value-lazy-map
                    :operator function
                    :value-n value-n
                    :inputs inputs
                    :shape shape
                    :identity identity
                    :number-of-values n-outputs
                    :ntype (petalisp.type-inference:ntype 't)))))))))
