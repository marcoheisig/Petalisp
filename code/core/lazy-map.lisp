;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun lazy-map (shape function inputs)
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
         (make-instance 'lazy-map
           :operator function
           :inputs inputs
           :shape shape
           :ntype (first ntypes)))
       (lambda ()
         (make-instance 'lazy-map
           :operator function
           :inputs inputs
           :shape shape
           :ntype (petalisp.type-inference:ntype 't))))))

(defun lazy-multiple-value-map (n-outputs shape function inputs)
  (if (empty-shape-p shape)
      (empty-arrays n-outputs)
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
         (let ((inputs (list (make-instance 'lazy-multiple-value-map
                               :operator function
                               :inputs inputs
                               :shape shape
                               :number-of-values n-outputs
                               :ntype (petalisp.type-inference:ntype t)))))
           (values-list
            (loop for ntype in ntypes
                  for value-n from 0
                  collect
                  (make-instance 'lazy-multiple-value-ref
                    :value-n value-n
                    :inputs inputs
                    :shape shape
                    :ntype ntype)))))
       (lambda ()
         (let ((inputs (list (make-instance 'lazy-multiple-value-map
                               :operator function
                               :inputs inputs
                               :shape shape
                               :number-of-values n-outputs
                               :ntype (petalisp.type-inference:ntype t)))))
           (values-list
            (loop for value-n from 0 below n-outputs
                  collect
                  (make-instance 'lazy-multiple-value-ref
                    :value-n value-n
                    :inputs inputs
                    :shape shape
                    :ntype (petalisp.type-inference:ntype t)))))))))
