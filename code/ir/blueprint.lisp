;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; A blueprint is like an s-expression, but made of ucons cells instead of
;;; cons cells.  It describes the computational behavior of a kernel, up to
;;; some variables.  Since ucons cells are immutable, and since any two
;;; ucons cells with the same contents are EQ, the blueprint of a kernel
;;; can be used as a key for caching information about a kernel.
;;;
;;; The blueprint grammar is:
;;;
;;; <kernel> := [[<range>*] [<buffer>*] [<buffer>*] <instruction>*]
;;;
;;; <range> := :contiguous | :strided
;;;
;;; <buffer> := [ntype rank]
;;;
;;; <instruction> := [:call number-of-values {operator | NIL} <input>*] |
;;;                  [:load source-buffer-number <iref>*] |
;;;                  [:store <input> target-buffer-number <iref>*] |
;;;                  [:iref <iref>]
;;;
;;; <input> := [value-n instruction-number]
;;;
;;; <iref> := [permutation { scaling | NIL } { offset | NIL }]

(defun kernel-blueprint (kernel)
  (ucons:ulist*
   (iteration-space-blueprint (kernel-iteration-space kernel))
   (buffer-blueprints (kernel-targets kernel))
   (buffer-blueprints (kernel-sources kernel))
   (instruction-blueprints kernel)))

(defun iteration-space-blueprint (iteration-space)
  (ucons:umapcar #'range-blueprint (shape-ranges iteration-space)))

(defun range-blueprint (range)
  (if (= 1 (range-step range))
      :contiguous
      :strided))

(defun buffer-blueprints (entries)
  (labels ((blueprint (entries)
             (if (null entries)
                 '()
                 (ucons:ucons
                  (buffer-blueprint (car (first entries)))
                  (blueprint (rest entries))))))
    (blueprint entries)))

(defun buffer-blueprint (buffer)
  (ucons:ulist
   (buffer-ntype buffer)
   (shape-rank (buffer-shape buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction Blueprints

(defun instruction-blueprints (kernel)
  (let ((vector (kernel-instruction-vector kernel))
        (result '()))
    (loop for index from (1- (length vector)) downto 0 do
      (let ((instruction (svref vector index)))
        (setf result (ucons:ucons (instruction-blueprint instruction kernel) result))))
    result))

(defgeneric instruction-blueprint (instruction kernel))

(defmethod instruction-blueprint
    ((call-instruction call-instruction)
     (kernel kernel))
  (ucons:ulist*
   :call
   (call-instruction-number-of-values call-instruction)
   (operator-blueprint (call-instruction-operator call-instruction))
   (ucons:umapcar #'value-blueprint (instruction-inputs call-instruction))))

(defmethod instruction-blueprint
    ((load-instruction load-instruction)
     (kernel kernel))
  (ucons:ulist*
   :load
   (position (load-instruction-buffer load-instruction)
             (kernel-sources kernel)
             :key #'car :test #'eq)
   (transformation-blueprint (instruction-transformation load-instruction))))

(defmethod instruction-blueprint
    ((store-instruction store-instruction)
     (kernel kernel))
  (ucons:ulist*
   :store
   (value-blueprint (first (instruction-inputs store-instruction)))
   (position (store-instruction-buffer store-instruction)
             (kernel-targets kernel)
             :key #'car :test #'eq)
   (transformation-blueprint
    (instruction-transformation store-instruction))))

(defmethod instruction-blueprint
    ((iref-instruction iref-instruction)
     (kernel kernel))
  (ucons:ulist*
   :iref
   (transformation-blueprint
    (instruction-transformation iref-instruction))))

(defun operator-blueprint (operator)
  (if (symbolp operator)
      operator
      'nil))

(defun value-blueprint (value)
  (destructuring-bind (value-n . instruction) value
    (ucons:ulist value-n (instruction-number instruction))))

(defun transformation-blueprint (transformation)
  (let ((result '()))
    (map-transformation-outputs
     (lambda (output-index input-index scaling offset)
       (declare (ignore output-index))
       (setf result (ucons:ucons
                     (ucons:ulist
                      input-index
                      (scaling-blueprint scaling)
                      (offset-blueprint offset))
                     result)))
     transformation
     :from-end t)
    result))

(defun scaling-blueprint (scaling)
  (if (<= -2 scaling 2)
      scaling
      nil))

(defun offset-blueprint (offset)
  (if (<= -3 offset 3)
      offset
      nil))
