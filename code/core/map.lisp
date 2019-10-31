;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(declaim (inline α))
(defun α (function &rest arrays)
  (declare (dynamic-extent arrays))
  (multiple-value-bind (inputs shape)
      (broadcast-list-of-arrays arrays)
    (α-aux 1 shape (coerce function 'function) inputs)))

(declaim (inline α*))
(defun α* (n-values function &rest arrays)
  (declare (petalisp.type-inference:multiple-value-count n-values)
           (dynamic-extent arrays))
  (multiple-value-bind (inputs shape)
      (broadcast-list-of-arrays arrays)
    (α-aux n-values shape (coerce function 'function) inputs)))

(declaim (notinline α-aux))
(defun α-aux (n-outputs shape function inputs)
  (if (null shape)
      (empty-arrays n-outputs)
      (petalisp.type-inference:specialize
       function
       inputs
       #'ntype
       (lambda (constant)
         (reshape constant shape))
       (lambda (ntypes function inputs)
         (values-list
          (loop for ntype in ntypes
                for value-n from 0
                collect
                (make-instance 'application
                  :operator function
                  :value-n value-n
                  :inputs inputs
                  :shape shape
                  :ntype ntype))))
       (lambda ()
         (values-list
          (loop for value-n below n-outputs
                collect
                (make-instance 'application
                  :operator function
                  :value-n value-n
                  :inputs inputs
                  :shape shape
                  :ntype (petalisp.type-inference:ntype 't))))))))
