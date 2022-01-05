;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(declaim (inline lazy-map))
(defun lazy-map (function inputs)
  (lazy-multiple-value-map function 1 inputs))

(defun lazy-multiple-value-map
    (function n-outputs inputs
     &aux
       (shape (lazy-array-shape (first inputs))))
  (declare (type (or symbol function) function)
           (unsigned-byte n-outputs)
           (shape shape)
           (list inputs))
  (labels
      ((wrap-constant (constant)
         (lazy-ref
          (lazy-array-from-scalar constant)
          shape
          (make-transformation
           :input-rank (shape-rank shape)
           :output-rank 0)))
       (wrap-function (ntypes operator inputs)
         (trivia:match ntypes
           ;; Zero return values.
           ((list)
            (values))
           ;; One return value.
           ((list ntype)
            (make-lazy-array
             :shape shape
             :ntype ntype
             :depth (1+ (maxdepth inputs))
             :delayed-action
             (make-delayed-map
              :operator operator
              :inputs inputs)))
           ;; More than one return value.
           (_
            (let* ((depth (1+ (maxdepth inputs)))
                   (default-ntype (petalisp.type-inference:ntype 'null))
                   (ntypes (replace (make-list n-outputs :initial-element default-ntype)
                                    ntypes))
                   (input
                     (make-lazy-array
                      :shape shape
                      :ntype nil
                      :depth depth
                      :refcount (length ntypes)
                      :delayed-action
                      (make-delayed-multiple-value-map
                       :operator operator
                       :inputs inputs
                       :ntypes ntypes))))
              (values-list
               (loop for ntype in ntypes
                     for value-n from 0
                     collect
                     (make-lazy-array
                      :shape shape
                      :ntype ntype
                      :depth (1+ depth)
                      :delayed-action
                      (make-delayed-nth-value
                       :number value-n
                       :input input))))))))
       (default ()
         (wrap-function
          (make-list n-outputs :initial-element (petalisp.type-inference:ntype t))
          function
          inputs)))
    (if (empty-shape-p shape)
        (empty-lazy-arrays n-outputs shape)
        ;; The type inference does most of the work here if we tell it how
        ;; to obtain the ntype of a lazy array and how to represent
        ;; constants and function calls as lazy arrays.  It is almost as if
        ;; the type inference was written for this very case :)
        (petalisp.type-inference:specialize
         function
         inputs
         #'lazy-array-ntype #'wrap-constant #'wrap-function #'default))))
