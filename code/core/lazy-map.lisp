;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun lazy-map (function shape inputs)
  (lazy-multiple-value-map function 1 shape inputs))

(defun lazy-multiple-value-map (function n-outputs shape inputs)
  (check-type n-outputs unsigned-byte)
  (check-type shape shape)
  (if (empty-shape-p shape)
      (empty-arrays n-outputs)
      (case n-outputs
        ;; Handle the case of a lazy map with zero outputs.
        (0 (values))
        ;; Handle the case of a lazy map with a single output.  The type
        ;; inference does most of the work here if we tell it how to obtain
        ;; the ntype of an array and if we tell it how to represent
        ;; constants and function calls as lazy arrays.  It is almost as if
        ;; the type inference was written for this very case :)
        (1 (petalisp.type-inference:specialize
            function
            inputs
            #'element-ntype
            (lambda (constant)
              (lazy-ref
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
                :ntype (petalisp.type-inference:ntype 't)))))
        ;; Handle the case of a lazy map with multiple outputs.  This case
        ;; is special in that we don't just generate a possibly specialized
        ;; tree of lazy arrays representing the operation, but also one
        ;; instance of a lazy-multiple-value-ref for each of the outputs.
        (otherwise
         (petalisp.type-inference:specialize
          function
          inputs
          #'element-ntype
          (lambda (constant)
            (lazy-ref
             (make-scalar-immediate constant)
             shape
             (make-transformation
              :input-rank (shape-rank shape)
              :output-rank 0)))
          (lambda (ntypes function inputs)
            (let* ((default-ntype (petalisp.type-inference:ntype 'null))
                   (ntype (make-list n-outputs :initial-element default-ntype))
                   (inputs (list (make-instance 'lazy-multiple-value-map
                                   :operator function
                                   :inputs inputs
                                   :shape shape
                                   :number-of-values n-outputs
                                   :ntype (replace ntype ntypes)))))
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
            (let* ((default-ntype (petalisp.type-inference:ntype t))
                   (ntype (make-list n-outputs :initial-element default-ntype))
                   (inputs (list (make-instance 'lazy-multiple-value-map
                                   :operator function
                                   :inputs inputs
                                   :shape shape
                                   :number-of-values n-outputs
                                   :ntype ntype))))
              (values-list
               (loop for value-n from 0 below n-outputs
                     collect
                     (make-instance 'lazy-multiple-value-ref
                       :value-n value-n
                       :inputs inputs
                       :shape shape
                       :ntype (petalisp.type-inference:ntype t)))))))))))
