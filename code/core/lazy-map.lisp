;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(declaim (inline lazy-map))
(defun lazy-map (function inputs)
  (lazy-multiple-value-map function 1 inputs))

(defun lazy-multiple-value-map
    (function n-outputs inputs
     &aux (shape (if (null inputs)
                     (make-shape '())
                     (lazy-array-shape (first inputs)))))
  (declare (type (or symbol function) function)
           (unsigned-byte n-outputs)
           (shape shape)
           (list inputs))
  (labels ((wrapper-nth-value (n lazy-array)
             (let ((delayed-action (lazy-array-delayed-action lazy-array)))
               (typecase delayed-action
                 (delayed-multiple-value-map
                  (let* ((values-ntype (delayed-multiple-value-map-values-ntype delayed-action))
                         (ntype (typo:values-ntype-nth-value-ntype n values-ntype)))
                    (if (typo:eql-ntype-p ntype)
                        (wrap-constant (typo:eql-ntype-object ntype))
                        (make-lazy-array
                         :shape shape
                         :ntype ntype
                         :depth (1+ (lazy-array-depth lazy-array))
                         :delayed-action
                         (make-delayed-nth-value
                          :number n
                          :input lazy-array)))))
                 (otherwise
                  (if (= n 0)
                      lazy-array
                      (wrap-constant nil))))))
           (wrapper-primary-value (lazy-array)
             (wrapper-nth-value 0 lazy-array))
           (wrapper-nth-value-ntype (n lazy-array)
             (let ((delayed-action (lazy-array-delayed-action lazy-array)))
               (typecase delayed-action
                 (delayed-multiple-value-map
                  (typo:values-ntype-nth-value-ntype
                   n
                   (delayed-multiple-value-map-values-ntype delayed-action)))
                 (otherwise
                  (if (= n 0)
                      (lazy-array-ntype lazy-array)
                      (typo:ntype-of nil))))))
           (wrap-constant (constant)
             (lazy-ref
              (lazy-array-from-scalar constant)
              shape
              (make-transformation
               :input-rank (shape-rank shape)
               :output-rank 0)))
           (wrap-function (fnrecord input-wrappers required optional rest)
             (declare (typo:fnrecord fnrecord)
                      (list input-wrappers required optional)
                      (type (or null typo:ntype) rest))
             (let* ((inputs (mapcar #'wrapper-primary-value input-wrappers))
                    (depth (1+ (maxdepth inputs)))
                    (n-required (length required))
                    (n-optional (length optional)))
               (if (and (= 1 n-required) (= 0 n-optional) (not rest))
                   (make-lazy-array
                    :shape shape
                    :ntype (first required)
                    :depth depth
                    :delayed-action
                    (make-delayed-map
                     :fnrecord fnrecord
                     :inputs inputs))
                   (make-lazy-array
                    :shape shape
                    :ntype (typo:empty-ntype)
                    :depth depth
                    :delayed-action
                    (make-delayed-multiple-value-map
                     :fnrecord fnrecord
                     :inputs inputs
                     :values-ntype
                     (typo:make-values-ntype required optional rest)))))))
    (if (empty-shape-p shape)
        (empty-lazy-arrays n-outputs shape)
        (let ((wrapper (typo:specialize
                        function
                        inputs
                        :wrap-constant #'wrap-constant
                        :wrap-function #'wrap-function
                        :wrapper-nth-value-ntype #'wrapper-nth-value-ntype)))
          (values-list
           (loop for n below n-outputs
                 collect (wrapper-nth-value n wrapper)))))))
