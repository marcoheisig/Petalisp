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

(defvar *small-kernel-p* nil
  "Whether a kernel is so small that there is no need including every detail in its
blueprint.")

(defun kernel-blueprint (kernel)
  "Returns a utree that represents all information necessary to generate a
high-performance implementation of KERNEL.  Identical blueprints are EQ,
which makes them ideal for caching."
  (let ((*small-kernel-p* (<= (shape-size (kernel-iteration-space kernel)) 64)))
    (ucons:ulist*
     (iteration-space-blueprint (kernel-iteration-space kernel))
     (buffer-stencils-alist-blueprint (kernel-targets kernel))
     (buffer-stencils-alist-blueprint (kernel-sources kernel))
     (instruction-blueprints kernel))))

(defun iteration-space-blueprint (iteration-space)
  (ranges-blueprint (shape-ranges iteration-space)))

(defun ranges-blueprint (ranges)
  (cond ((null ranges)
         '())
        ((and (= 1 (range-step (first ranges)))
              (not *small-kernel-p*))
         (ucons:ulist* :contiguous (ranges-blueprint (rest ranges))))
        (t
         (ucons:ulist* :strided (ranges-blueprint (rest ranges))))))

(defun buffer-stencils-alist-blueprint (alist)
  (trivia:ematch alist
    ((list) '())
    ((list* (list* buffer stencils) rest)
     (ucons:ulist*
      (ucons:ulist*
       (buffer-ntype buffer)
       (stencils-blueprint stencils))
      (buffer-stencils-alist-blueprint rest)))))

(defun stencils-blueprint (stencils)
  (trivia:ematch stencils
    ((list) '())
    ((list* stencil more-stencils)
     (ucons:ucons
      (transformation-blueprint
       (instruction-transformation
        (first (stencil-instructions stencil))))
      (stencils-blueprint more-stencils)))))

(defun transformation-blueprint (transformation)
  (let* ((output-rank (transformation-output-rank transformation))
         (output-mask (transformation-output-mask transformation))
         (scalings (transformation-scalings transformation))
         (result '()))
    (loop for index from (1- output-rank) downto 0 do
      (ucons:upush
       (ucons:ulist
        (aref output-mask index)
        (if *small-kernel-p*
            :any
            (let ((scaling (aref scalings index)))
              (case scaling
                ((0 1 2 4 8 16 32) scaling)
                (otherwise :any)))))
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
        (when (member load-instruction (stencil-instructions stencil))
          (return-from instruction-blueprint
            (ucons:ulist*
             :load
             buffer-number
             stencil-number
             (stencil-instruction-offsets stencil load-instruction)))))))
  (error "Malformed load instruction: ~S" load-instruction))

(defmethod instruction-blueprint
    ((store-instruction store-instruction)
     (kernel kernel))
  (loop for (buffer . stencils) in (kernel-targets kernel) for buffer-number from 0 do
    (when (eq buffer (store-instruction-buffer store-instruction))
      (loop for stencil in stencils for stencil-number from 0 do
        (when (member store-instruction (stencil-instructions stencil))
          (return-from instruction-blueprint
            (ucons:ulist*
             :store
             (value-blueprint (first (instruction-inputs store-instruction)))
             buffer-number
             stencil-number
             (stencil-instruction-offsets stencil store-instruction)))))))
  (error "Malformed store instruction: ~S" store-instruction))

(defun stencil-instruction-offsets (stencil instruction)
  (declare (stencil stencil) (load-or-store-instruction instruction))
  (let ((center (stencil-center stencil))
        (offsets (transformation-offsets (instruction-transformation instruction)))
        (result '()))
    (loop for index from (1- (stencil-output-rank stencil)) downto 0 do
      (ucons:upush
       (- (aref offsets index)
          (aref center index))
       result))
    result))

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
