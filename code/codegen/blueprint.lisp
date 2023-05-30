;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.codegen)

;;; A blueprint is like an s-expression, but made of ucons cells instead of
;;; cons cells.  It describes the computational behavior of a kernel, up to
;;; some variables.  Since ucons cells are immutable, and since any two
;;; ucons cells with the same contents are EQ, the blueprint of a kernel
;;; can be used as a key for caching information about a kernel.
;;;
;;; The blueprint grammar is:
;;;
;;; <kernel> := [<iteration-space> <targets> <sources> <instruction>*]
;;;
;;; <iteration-space> := [{:contiguous | :strided}*]
;;;
;;; <targets> := [[<ntype> <transformation>*]*]
;;;
;;; <sources> := [[<ntype> <transformation>*]*]
;;;
;;; <transformation> := [<iref>*]
;;;
;;; <iref> := [<permutation> { <scaling> | :any }]
;;;
;;; <instruction> := [:call number-of-values { function-name | :any } <input>*] |
;;;                  [:store <input> <target-number> <target-transformation-number>] |
;;;                  [:load <source-number> <source-transformation-number> <offset>*] |
;;;                  [:iref <iref>]
;;;
;;; <input> := [<value-n> <instruction-number>]

(defun kernel-blueprint (kernel)
  "Returns a utree that represents all information necessary to generate a
high-performance implementation of KERNEL.  Identical blueprints are EQ,
which makes them ideal for caching."
  (ucons:ulist*
   (iteration-space-blueprint (kernel-iteration-space kernel))
   (target-blueprints (kernel-targets kernel))
   (source-blueprints (kernel-sources kernel))
   (instruction-blueprints kernel)))

(defun iteration-space-blueprint (iteration-space)
  (ranges-blueprint (shape-ranges iteration-space)))

(defun ranges-blueprint (ranges)
  (if (null ranges)
      '()
      (ucons:ucons
       (if (or (= 0 (range-size (first ranges)))
               (= 1 (range-step (first ranges))))
           :contiguous
           :strided)
       (ranges-blueprint (rest ranges)))))

(defun target-blueprints (targets)
  (if (null targets)
      '()
      (ucons:ucons
       (target-blueprint (first targets))
       (target-blueprints (rest targets)))))

(defun target-blueprint (target)
  (destructuring-bind (buffer . store-instructions) target
    (ucons:ulist*
     (buffer-ntype buffer)
     (labels ((transformations (store-instructions)
                (if (null store-instructions)
                    '()
                    (ucons:ucons
                     (transformation-blueprint
                      (store-instruction-transformation
                       (first store-instructions)))
                     (transformations
                      (rest store-instructions))))))
       (transformations store-instructions)))))

(defun source-blueprints (sources)
  (if (null sources)
      '()
      (ucons:ucons
       (source-blueprint (first sources))
       (source-blueprints (rest sources)))))

(defun source-blueprint (source)
  (destructuring-bind (buffer . stencils) source
    (ucons:ulist*
     (buffer-ntype buffer)
     (labels ((transformations (stencils)
                (if (null stencils)
                    '()
                    (ucons:ucons
                     (transformation-blueprint
                      (load-instruction-transformation
                       (first
                        (stencil-load-instructions
                         (first stencils)))))
                     (transformations
                      (rest stencils))))))
       (transformations stencils)))))

(defun transformation-blueprint (transformation)
  (let* ((output-rank (transformation-output-rank transformation))
         (output-mask (transformation-output-mask transformation))
         (scalings (transformation-scalings transformation))
         (result '()))
    (loop for index from (1- output-rank) downto 0 do
      (ucons:upush
       (ucons:ulist
        (aref output-mask index)
        (let ((scaling (aref scalings index)))
          (case scaling
            ((0 1 2 4 8 16 32) scaling)
            (otherwise :any))))
       result))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction Blueprints

(defun instruction-blueprints (kernel)
  (let ((vector (kernel-instruction-vector kernel))
        (result '()))
    (loop for index from (1- (length vector)) downto 0 do
      (let ((instruction (svref vector index)))
        (ucons:upush (instruction-blueprint instruction kernel) result)))
    result))

(defgeneric instruction-blueprint (instruction kernel))

(defmethod instruction-blueprint
    ((call-instruction call-instruction)
     (kernel kernel))
  (ucons:ulist*
   :call
   (call-instruction-number-of-values call-instruction)
   (fnrecord-blueprint (call-instruction-fnrecord call-instruction))
   (ucons:umapcar #'value-blueprint (instruction-inputs call-instruction))))

(defmethod instruction-blueprint
    ((load-instruction load-instruction)
     (kernel kernel))
  (loop for (buffer . stencils) in (kernel-sources kernel) for buffer-number from 0 do
    (when (eq buffer (load-instruction-buffer load-instruction))
      (loop for stencil in stencils for stencil-number from 0 do
        (when (member load-instruction (stencil-load-instructions stencil))
          (let ((center (stencil-center stencil))
                (offsets (transformation-offsets (load-instruction-transformation load-instruction)))
                (tail '()))
            (loop for index from (1- (stencil-output-rank stencil)) downto 0 do
              (ucons:upush
               (- (aref offsets index)
                  (aref center index))
               tail))
            (return-from instruction-blueprint
              (ucons:ulist* :load buffer-number stencil-number tail)))))))
  (error "Invalid load instruction: ~S" load-instruction))

(defmethod instruction-blueprint
    ((store-instruction store-instruction)
     (kernel kernel))
  (loop for (buffer . store-instructions) in (kernel-targets kernel) for buffer-number from 0 do
    (when (eq buffer (store-instruction-buffer store-instruction))
      (loop for other-store in store-instructions for store-number from 0 do
        (when (eq other-store store-instruction)
          (return-from instruction-blueprint
            (ucons:ulist
             :store
             (value-blueprint (first (instruction-inputs store-instruction)))
             buffer-number
             store-number))))))
  (error "Invalid store instruction: ~S" store-instruction))

(defmethod instruction-blueprint
    ((iref-instruction iref-instruction)
     (kernel kernel))
  (ucons:ulist*
   :iref
   (transformation-blueprint
    (instruction-transformation iref-instruction))))

(defun fnrecord-blueprint (fnrecord)
  (let ((name (typo:fnrecord-name fnrecord)))
    (if (typep name 'typo:function-name)
        name
        :any)))

(defun value-blueprint (value)
  (destructuring-bind (value-n . instruction) value
    (ucons:ulist value-n (instruction-number instruction))))
