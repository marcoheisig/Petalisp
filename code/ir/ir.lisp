;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

(defstruct (program
            (:predicate programp)
            (:constructor make-program))
  "A program is the largest organizational unit in Petalisp's IR and fully
describes the semantics of a particular graph of lazy arrays.  It has the
following slots:

- The initial task, which is the unique task in the program with zero
  predecessors.

- The final task, which is the unique task in the program with zero successors.

- An association list where each entry has a key that is a leaf buffer and a
  value that is the corresponding lazy array.

- A list of all root buffers of the program in the order that they were
  submitted for evaluation.

- A simple vector that contains all the tasks in the program sorted by their
  task number.

- The number of buffers in the program.

- The number of kernels in the program."
  (initial-task nil)
  (final-task nil)
  (leaf-alist '() :type list)
  (root-buffers '() :type list)
  (task-vector #() :type simple-vector)
  (number-of-buffers 0 :type (and unsigned-byte fixnum))
  (number-of-kernels 0 :type (and unsigned-byte fixnum)))

(declaim (inline program-number-of-tasks))
(defun program-number-of-tasks (program)
  (declare (program program))
  (length (program-task-vector program)))

(defstruct (task
            (:predicate taskp)
            (:constructor make-task))
  "A task is a collection of kernels that fully define a set of buffers.
The rules for task membership are:

1. All kernels writing to a buffer B with task T have task T.

2. All buffers written to by a kernel K with task T have task T.

3. A buffer that is used by a kernel in T and that depends on a buffer
   in T is also in T."
  (program '() :type program)
  ;; The tasks that must be completed before this task can run.
  (predecessors '() :type list)
  ;; The tasks that have this one as their predecessor.
  (successors '() :type list)
  ;; This task's kernels.
  (kernels '() :type list)
  ;; The buffers defined by this task.
  (defined-buffers '() :type list)
  ;; A number that is unique among all tasks in this program and less than
  ;; the number of tasks in the program.
  (number 0 :type (and unsigned-byte fixnum)))

(defstruct (buffer
            (:predicate bufferp)
            (:constructor make-buffer))
  "A buffer is a mapping from indices of a particular shape to memory locations of
a particular type.  It has the following slots:

- A shape that describes the size and virtual layout of the buffer.

- A conservative approximation of the type of each element of the buffer,
  represented as an ntype of the type inference library Typo.

- The depth of the lazy array corresponding to the buffer.

- The writers of the buffer, encoded as an association list where the key of
  each entry is a kernel and the value is the list of all the store
  instructions in that kernel that write to the buffer.

- The readers of the buffer, encoded as an association where the key of each
  entry is a kernel and the value is a list of all the load instructions in
  that kernel that load from the buffer.

- The task that contains all kernels that write into this buffer.

- The storage of the buffer, which is an opaque, backend-specific object.

- A number that is unique among all buffers in the program and less than the
  total number of buffers in the program."
  (shape nil :type shape)
  (ntype nil :type typo:ntype)
  (depth nil :type (and unsigned-byte fixnum))
  (writers '() :type list)
  (readers '() :type list)
  (task nil :type (or null task))
  (number 0 :type (and unsigned-byte fixnum))
  (storage nil))

(declaim (inline leaf-buffer-p))
(defun leaf-buffer-p (buffer)
  (null (buffer-writers buffer)))

(declaim (inline root-buffer-p))
(defun root-buffer-p (buffer)
  (null (buffer-readers buffer)))

(declaim (inline interior-buffer-p))
(defun interior-buffer-p (buffer)
  (not (or (leaf-buffer-p buffer)
           (root-buffer-p buffer))))

(declaim (inline buffer-size))
(defun buffer-size (buffer)
  (shape-size (buffer-shape buffer)))

(declaim (inline buffer-program))
(defun buffer-program (buffer)
  (declare (buffer buffer))
  (task-program (buffer-task buffer)))

(declaim (inline buffer-bits))
(defun buffer-bits (buffer)
 (* (typo:ntype-bits (buffer-ntype buffer))
     (shape-size (buffer-shape buffer))))

(defstruct (kernel
            (:predicate kernelp)
            (:constructor make-kernel))
  "A kernel represents a computation that executes some instructions once for
each element in its iteration space. It has the following slots:

- The iteration space, encoded as a shape.

- The instruction vector that contains the instructions of that kernel sorted
  by their instruction numbers.

- The sources of that kernel, encoded as an association list where the key of
  each entry is a buffer and where the value is a list of stencils of load
  instructions reading from that buffer.

- The targets of that kernel, encoded as an association list where the key of
  each entry is a buffer and where the value is a list of stencils of store
  instructions writing into that buffer.

- The task that contains this kernel.

- A number that is unique among all kernels in the program and less than the
  total number of kernels in the program."
  (iteration-space nil :type shape)
  (sources '() :type list)
  (targets '() :type list)
  (instruction-vector #() :type simple-vector)
  (task nil :type (or null task))
  (number 0 :type (and unsigned-byte fixnum))
  (data nil))

(declaim (inline kernel-program))
(defun kernel-program (kernel)
  (declare (kernel kernel))
  (task-program (kernel-task kernel)))

(defstruct (instruction
            (:predicate instructionp)
            (:copier nil)
            (:constructor nil))
  "An instruction describes part of the semantics of the kernel that contains it.
This abstract base class outlines behavior that is shared among all
instructions.  It prescribes two slots:

- The instruction number, which is an integer that is unique among all
  instructions of the containing kernel.  Instruction numbers are handed out in
  depth-first order of instruction dependencies, such that the roots (store
  instructions) have the highest numbers and the leaf nodes (load and iref
  instructions) have the lowest numbers.

- A list of inputs, whose elements are cons cells whose CDR is another
  instruction in the same kernel and whose CAR is an integer denoting which of
  the multiple values returned by the instruction in the CDR is being
  referenced.  An instruction with zero inputs is called a leaf instruction."
  (inputs '() :type list)
  ;; A number that is unique among all instructions of this kernel.
  (number 0 :type (and unsigned-byte fixnum)))

(defstruct (call-instruction
            (:include instruction)
            (:predicate call-instruction-p)
            (:copier nil)
            (:constructor make-call-instruction (number-of-values fnrecord inputs)))
  "A call instruction represents the application of a function to some
arguments.  It consists of an fnrecord from the type inference library Typo, an
integer that specifies the number of values produced by the call instruction,
and a list of inputs.  The values of a call instruction are obtained by
applying the fnrecord's function to the values of each of the call
instruction's inputs.  If the function application produces fewer values than
specified, the missing values are set to NIL."
  (fnrecord nil :type typo:fnrecord)
  (number-of-values nil :type (integer 0 (#.multiple-values-limit))))

(defun call-instruction-function (call-instruction)
  (typo:fnrecord-function
   (call-instruction-fnrecord call-instruction)))

(defstruct (iterating-instruction
            (:include instruction)
            (:predicate iterating-instruction-p)
            (:copier nil)
            (:constructor nil)
            (:conc-name instruction-))
  "The class of all instructions whose behavior directly depends on the
current position in the surrounding iteration space."
  (transformation nil :type transformation))

(defstruct (iref-instruction
            (:include iterating-instruction)
            (:predicate iref-instruction-p)
            (:copier nil)
            (:constructor make-iref-instruction
                (transformation)))
  "An iref instruction represents an access to elements of the iteration
space itself.  It contains a transformation that maps each index of the
iteration space to an index of rank one.  Its value is the single index
component of that rank one index.  An iref instruction has zero inputs.")

(defstruct (load-or-store-instruction
            (:include iterating-instruction)
            (:predicate load-or-store-instruction-p)
            (:copier nil)
            (:constructor nil)
            (:conc-name instruction-))
  (buffer nil :type buffer))

(defstruct (load-instruction
            (:include load-or-store-instruction)
            (:predicate load-instruction-p)
            (:copier nil)
            (:constructor %make-load-instruction
                (buffer transformation)))
  "A load instruction represents a read from main memory.  It consists of a buffer
that is being read from and a transformation that maps each index of the
iteration space to a position in that buffer.  A load instruction has zero
inputs and produces a single output that is the value that has been loaded.")

(defstruct (store-instruction
            (:include load-or-store-instruction)
            (:predicate store-instruction-p)
            (:copier nil)
            (:constructor %make-store-instruction
                (inputs buffer transformation)))
  "A store instruction represents a write to memory.  It consists of a buffer that
is being written to, a transformation that maps each index in the iteration
space to a location in that buffer, and a single input that is the value to be
written.  A store instruction returns zero values.")

(defun store-instruction-input (store-instruction)
  (declare (store-instruction store-instruction))
  (first (store-instruction-inputs store-instruction)))


(declaim (unsigned-byte *stencil-max-radius*))
(defparameter *stencil-max-radius* 7)

(defstruct (stencil
            (:predicate stencilp)
            (:copier nil)
            (:constructor %make-stencil))
  "A stencil is a set of either load or store instructions that all have the same
buffer, kernel, output mask, and scalings, and whose offsets are off by at most
*STENCIL-MAX-RADIUS* from the center of the stencil (measured in steps of the
corresponding range of the buffer being loaded from).  The center of a stencil
is a vector containing the averages of the offsets of its instructions rounded
to the nearest integer.

Stencils are crucial for reasoning about memory locality.  During buffer
partitioning, memory that is accessed by exactly one stencil per kernel can be
split by introducing ghost layers.  The maximum distance of any offset from the
stencil's center determines the number of ghost layers that have to be
introduced."
  ;; The center of a stencil is an array that contains the average of the
  ;; offsets of all its instructions.
  (center nil :type simple-vector)
  ;; A non-empty list of iterating instructions.
  (instructions nil :type (cons load-or-store-instruction list)))

(defun compute-stencil-center (instruction &rest more-instructions)
  "Returns a vector of offsets that is the weighted average of all the offsets of
all the supplied load or store instructions."
  (flet ((offsets (instruction)
           (transformation-offsets
            (instruction-transformation instruction))))
    (if (null more-instructions)
        (offsets instruction)
        (let* ((result (alexandria:copy-array (offsets instruction)))
               (count 1))
          (loop for instruction in more-instructions do
            (incf count)
            (let ((offsets (offsets instruction)))
              (assert (= (length offsets) (length result)))
              (loop for offset across offsets for index from 0 do
                (incf (aref result index) offset))))
          (loop for sum across result for index from 0 do
            (setf (aref result index)
                  (floor sum count)))
          result))))

(defun make-stencil (instructions)
  (%make-stencil
   :center (apply #'compute-stencil-center instructions)
   :instructions instructions))

(defun stencil-from-instruction (instruction)
  (declare (type load-or-store-instruction instruction))
  (%make-stencil
   :center (compute-stencil-center instruction)
   :instructions (list instruction)))

;;; Many stencil properties can be inferred directly from its first
;;; instruction, so we generate those repetitive readers with a macro.
(macrolet ((def (name (instruction-var) &body body)
             `(progn
                (declaim (inline ,name))
                (defun ,name (stencil)
                  (declare (stencil stencil))
                  (let ((,instruction-var (first (stencil-instructions stencil))))
                    ,@body)))))
  (def stencil-buffer (instruction)
    (instruction-buffer instruction))
  (def stencil-input-rank (instruction)
    (transformation-input-rank
     (instruction-transformation instruction)))
  (def stencil-output-rank (instruction)
    (transformation-output-rank
     (instruction-transformation instruction)))
  (def stencil-output-mask (instruction)
    (transformation-output-mask
     (instruction-transformation instruction)))
  (def stencil-scalings (instruction)
    (transformation-scalings
     (instruction-transformation instruction))))

(defun kernel-load-stencils (kernel buffer)
  (let ((entry (assoc buffer (kernel-sources kernel))))
    (etypecase entry
      (null '())
      (cons (cdr entry)))))

(defun kernel-store-stencils (kernel buffer)
  (let ((entry (assoc buffer (kernel-targets kernel))))
    (etypecase entry
      (null '())
      (cons (cdr entry)))))

(defun maybe-add-to-stencil (instruction stencil)
  (declare (load-or-store-instruction instruction))
  (let ((buffer (instruction-buffer instruction))
        (transformation (instruction-transformation instruction)))
    (when (and (eq (stencil-buffer stencil) buffer)
               (equalp (stencil-output-mask stencil)
                       (transformation-output-mask transformation))
               (equalp (stencil-scalings stencil)
                       (transformation-scalings transformation)))
      ;; Compute the center of a stencil with that instruction added.
      (let* ((ranges (shape-ranges (buffer-shape buffer)))
             (instructions (list* instruction (stencil-instructions stencil)))
             (center (apply #'compute-stencil-center instructions)))
        ;; Ensure that the new center is valid for all instructions, including
        ;; the one we are trying to add.
        (loop for instruction in instructions do
          (let* ((transformation (instruction-transformation instruction))
                 (offsets (transformation-offsets transformation)))
            (loop for offset1 across offsets
                  for offset2 across center
                  for range in ranges
                  do (unless (<= (abs (- offset2 offset1))
                                 (* *stencil-max-radius* (range-step range)))
                       (return-from maybe-add-to-stencil nil)))))
        ;; If control reaches this point, we know that the new center is valid.
        ;; We can now add the instruction to the stencil.
        (setf (stencil-center stencil) center)
        (setf (stencil-instructions stencil) instructions)
        t))))

(defun make-load-instruction (kernel buffer transformation)
  (let ((load-instruction (%make-load-instruction buffer transformation)))
    ;; Either add the load instruction to an existing stencil, or create a new
    ;; stencil containing that instruction and add it to the kernel.
    (block add-load-instruction-to-kernel
      (symbol-macrolet ((stencils (alexandria:assoc-value (kernel-sources kernel) buffer)))
        (dolist (stencil stencils)
          (when (maybe-add-to-stencil load-instruction stencil)
            (return-from add-load-instruction-to-kernel)))
        (push (stencil-from-instruction load-instruction)
              stencils)))
    ;; Add the load instruction to the buffer it reads from, too.
    (push load-instruction (alexandria:assoc-value (buffer-readers buffer) kernel))
    load-instruction))

(defun make-store-instruction (kernel input buffer transformation)
  (let ((store-instruction (%make-store-instruction (list input) buffer transformation)))
    ;; Either add the store instruction to an existing stencil, or create a new
    ;; stencil containing that instruction and add it to the kernel.
    (block add-store-instruction-to-kernel
      (symbol-macrolet ((stencils (alexandria:assoc-value (kernel-targets kernel) buffer)))
        (dolist (stencil stencils)
          (when (maybe-add-to-stencil store-instruction stencil)
            (return-from add-store-instruction-to-kernel)))
        (push (stencil-from-instruction store-instruction)
              stencils)))
    ;; Add the store instruction to the buffer it writes to, too.
    (push store-instruction (alexandria:assoc-value (buffer-writers buffer) kernel))
    store-instruction))

(defgeneric instruction-number-of-values (instruction)
  (:method ((call-instruction call-instruction))
    (call-instruction-number-of-values call-instruction))
  (:method ((iref-instruction iref-instruction))
    1)
  (:method ((load-instruction load-instruction))
    1)
  (:method ((store-instruction store-instruction))
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printing

(defmethod print-object ((program program) stream)
  (print-unreadable-object (program stream :type t :identity t)
    (format stream "~S" (program-task-vector program))))

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type t :identity t)
    (format stream "~S" (task-defined-buffers task))))

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream :type t :identity t)
    (format stream "~S ~S"
            (typo:ntype-type-specifier (buffer-ntype buffer))
            (buffer-shape buffer))))

(defmethod print-object ((kernel kernel) stream)
  (print-unreadable-object (kernel stream :type t :identity t)
    (format stream "~S"
            (kernel-iteration-space kernel))))

;;; This function is used during printing, to avoid excessive circularity.
(defun simplify-input (input)
  (destructuring-bind (value-n . instruction) input
    (cons value-n (instruction-number instruction))))

(defmethod print-object ((call-instruction call-instruction) stream)
  (print-unreadable-object (call-instruction stream :type t)
    (format stream "~S ~S ~S"
            (instruction-number call-instruction)
            (typo:fnrecord-function (call-instruction-fnrecord call-instruction))
            (mapcar #'simplify-input (instruction-inputs call-instruction)))))

(defmethod print-object ((load-instruction load-instruction) stream)
  (print-unreadable-object (load-instruction stream :type t)
    (format stream "~S ~S ~S"
            (instruction-number load-instruction)
            :buffer ;(load-instruction-buffer load-instruction)
            (instruction-transformation load-instruction))))

(defmethod print-object ((store-instruction store-instruction) stream)
  (print-unreadable-object (store-instruction stream :type t)
    (format stream "~S ~S ~S ~S"
            (instruction-number store-instruction)
            (simplify-input (first (instruction-inputs store-instruction)))
            :buffer ;(store-instruction-buffer store-instruction)
            (instruction-transformation store-instruction))))

(defmethod print-object ((iref-instruction iref-instruction) stream)
  (print-unreadable-object (iref-instruction stream :type t)
    (format stream "~S ~S"
            (instruction-number iref-instruction)
            (instruction-transformation iref-instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mapping Functions

(declaim (inline map-program-tasks))
(defun map-program-tasks (function program)
  (declare (program program))
  (loop for task across (program-task-vector program) do
    (funcall function task)))

(declaim (inline map-task-successors))
(defun map-task-successors (function task)
  (mapc function (task-successors task)))

(declaim (inline map-task-predecessors))
(defun map-task-predecessors (function task)
  (mapc function (task-predecessors task)))

(declaim (inline map-task-kernels))
(defun map-task-kernels (function task)
  (mapc function (task-kernels task)))

(declaim (inline map-task-defined-buffers))
(defun map-task-defined-buffers (function task)
  (mapc function (task-defined-buffers task)))

(declaim (inline map-program-buffers))
(defun map-program-buffers (function program)
  (declare (program program))
  (map-program-tasks
   (lambda (task)
     (map-task-defined-buffers function task))
   program))

(declaim (inline map-program-kernels))
(defun map-program-kernels (function program)
  (declare (program program))
  (map-program-tasks
   (lambda (task)
     (map-task-kernels function task))
   program))

(declaim (inline map-buffer-inputs))
(defun map-buffer-inputs (function buffer)
  (declare (function function)
           (buffer buffer))
  (loop for (kernel . nil) in (buffer-writers buffer) do
    (funcall function kernel))
  buffer)

(declaim (inline map-buffer-outputs))
(defun map-buffer-outputs (function buffer)
  (declare (function function)
           (buffer buffer))
  (loop for (kernel . nil) in (buffer-readers buffer) do
    (funcall function kernel))
  buffer)

(declaim (inline map-buffer-load-instructions))
(defun map-buffer-load-instructions (function buffer)
  (declare (function function)
           (buffer buffer))
  (loop for (nil . load-instructions) in (buffer-readers buffer) do
    (loop for load-instruction in load-instructions do
      (funcall function load-instruction)))
  buffer)

(declaim (inline map-buffer-store-instructions))
(defun map-buffer-store-instructions (function buffer)
  (declare (function function)
           (buffer buffer))
  (loop for (nil . store-instructions) in (buffer-writers buffer) do
    (loop for store-instruction in store-instructions do
      (funcall function store-instruction)))
  buffer)

(declaim (inline map-kernel-inputs))
(defun map-kernel-inputs (function kernel)
  (declare (function function)
           (kernel kernel))
  (loop for (buffer . nil) in (kernel-sources kernel) do
    (funcall function buffer))
  kernel)

(declaim (inline map-kernel-outputs))
(defun map-kernel-outputs (function kernel)
  (declare (function function)
           (kernel kernel))
  (loop for (buffer . nil) in (kernel-targets kernel) do
    (funcall function buffer))
  kernel)

(declaim (inline map-kernel-stencils))
(defun map-kernel-stencils (function kernel)
  (declare (function function)
           (kernel kernel))
  (loop for (nil . stencils) in (kernel-targets kernel) do
    (mapc function stencils))
  (loop for (nil . stencils) in (kernel-sources kernel) do
    (mapc function stencils))
  kernel)

(declaim (inline map-kernel-load-instructions))
(defun map-kernel-load-instructions (function kernel)
  (declare (function function)
           (kernel kernel))
  (loop for (nil . stencils) in (kernel-sources kernel) do
    (loop for stencil in stencils do
      (loop for load-instruction in (stencil-instructions stencil) do
        (funcall function load-instruction))))
  kernel)

(declaim (inline map-kernel-store-instructions))
(defun map-kernel-store-instructions (function kernel)
  (declare (function function)
           (kernel kernel))
  (loop for (nil . stencils) in (kernel-targets kernel) do
    (loop for stencil in stencils do
      (loop for store-instruction in (stencil-instructions stencil) do
        (funcall function store-instruction))))
  kernel)

(declaim (inline map-instruction-inputs))
(defun map-instruction-inputs (function instruction)
  (declare (function function)
           (instruction instruction))
  (loop for (nil . input) in (instruction-inputs instruction) do
    (funcall function input)))

(defun map-buffers-and-kernels (buffer-fn kernel-fn root-buffers)
  (unless (null root-buffers)
    (map-program-tasks
     (lambda (task)
       (map-task-defined-buffers buffer-fn task)
       (map-task-kernels kernel-fn task))
     (task-program (buffer-task (first root-buffers))))))

(defun map-buffers (function root-buffers)
  (map-buffers-and-kernels function #'identity root-buffers))

(defun map-kernels (function root-buffers)
  (map-buffers-and-kernels #'identity function root-buffers))

(declaim (inline map-kernel-instructions))
(defun map-kernel-instructions (function kernel)
  (let ((vector (kernel-instruction-vector kernel)))
    (declare (simple-vector vector))
    (map nil function vector)))

(declaim (inline map-stencil-instructions))
(defun map-stencil-instructions (function stencil)
  (mapc function (stencil-instructions stencil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Do Macros

(macrolet ((def (name var thing mapper)
             `(defmacro ,name ((,var ,thing &optional result) &body body)
                (check-type ,var symbol)
                `(block nil
                   (,',mapper (lambda (,,var) ,@body) ,,thing)
                   ,result))))
  (def do-program-tasks task program map-program-tasks)
  (def do-task-successors successor task map-task-successors)
  (def do-task-predecessors predecessor task map-task-predecessors)
  (def do-task-kernels kernel task map-task-kernels)
  (def do-task-defined-buffers defined-buffer task map-task-defined-buffers)
  (def do-program-buffers buffer program map-program-buffers)
  (def do-program-kernels kernel program map-program-kernels)
  (def do-buffer-inputs kernel buffer map-buffer-inputs)
  (def do-buffer-outputs kernel buffer map-buffer-outputs)
  (def do-buffer-load-instructions load-instruction buffer map-buffer-load-instructions)
  (def do-buffer-store-instructions store-instruction buffer map-buffer-store-instructions)
  (def do-kernel-inputs buffer kernel map-kernel-inputs)
  (def do-kernel-outputs buffer kernel map-kernel-outputs)
  (def do-kernel-stencils stencil kernel map-kernel-stencils)
  (def do-kernel-load-instructions load-instruction kernel map-kernel-load-instructions)
  (def do-kernel-store-instructions store-instruction kernel map-kernel-store-instructions)
  (def do-stencil-instructions load-or-store-instruction stencil map-stencil-instructions)
  (def do-instruction-inputs input instruction map-instruction-inputs)
  (def do-kernel-instructions instruction kernel map-kernel-instructions))

;;; Apply FUNCTION to each list of non-leaf buffers that have the same
;;; shape and element type.
(defun map-program-buffer-groups (function program)
  (let ((buffers '()))
    (do-program-buffers (buffer program)
      (unless (leaf-buffer-p buffer)
        (push buffer buffers)))
    (setf buffers (stable-sort buffers #'< :key (alexandria:compose #'typo:ntype-index #'buffer-ntype)))
    (setf buffers (stable-sort buffers #'shape< :key #'buffer-shape))
    (loop until (null buffers) do
      (let* ((buffer (first buffers))
             (shape (buffer-shape buffer))
             (ntype (buffer-ntype buffer))
             (last buffers))
        ;; Locate the last cons cell whose CAR is a buffer with the same
        ;; shape and ntype.
        (loop for cdr = (cdr last)
              while (consp cdr)
              while (let* ((other-buffer (car cdr))
                           (other-shape (buffer-shape other-buffer))
                           (other-ntype (buffer-ntype other-buffer)))
                      (and (shape= shape other-shape)
                           (typo:ntype= ntype other-ntype)))
              do (setf last (cdr last)))
        ;; Destructively cut the list of buffers right after that last
        ;; cons.
        (let ((rest (cdr last)))
          (setf (cdr last) nil)
          (funcall function buffers)
          (setf buffers rest))))))

(defmacro do-program-buffer-groups ((buffers program &optional result) &body body)
  (check-type buffers symbol)
  `(block nil
     (map-program-buffer-groups (lambda (,buffers) ,@body) ,program)
     ,result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transforming Kernels and Buffers

(defgeneric transform-instruction-input (instruction transformation)
  (:method ((instruction instruction)
            (transformation transformation))
    (values))
  (:method ((instruction iterating-instruction)
            (transformation transformation))
    (setf (instruction-transformation instruction)
          (compose-transformations
           (instruction-transformation instruction)
           transformation))))

(defgeneric transform-instruction-output (instruction transformation)
  (:method ((instruction instruction)
            (transformation transformation))
    (values))
  (:method ((instruction iterating-instruction)
            (transformation transformation))
    (setf (instruction-transformation instruction)
          (compose-transformations
           transformation
           (instruction-transformation instruction)))))

(defun transform-buffer (buffer transformation)
  (declare (buffer buffer)
           (transformation transformation))
  (setf (buffer-shape buffer)
        (transform-shape (buffer-shape buffer) transformation))
  ;; After rotating a buffer, rotate all loads and stores referencing the
  ;; buffer to preserve the semantics of the IR.
  (map-buffer-store-instructions
   (lambda (store-instruction)
     (transform-instruction-output store-instruction transformation))
   buffer)
  (map-buffer-load-instructions
   (lambda (load-instruction)
     (transform-instruction-output load-instruction transformation))
   buffer)
  buffer)

(defun transform-kernel (kernel transformation)
  (declare (kernel kernel)
           (transformation transformation))
  (unless (transformation-identityp transformation)
    (setf (kernel-iteration-space kernel)
          (transform-shape (kernel-iteration-space kernel) transformation))
    (let ((inverse (invert-transformation transformation)))
      (do-kernel-instructions (instruction kernel)
        (transform-instruction-input instruction inverse))))
  (do-kernel-stencils (stencil kernel)
    (setf (stencil-center stencil)
          (apply #'compute-stencil-center (stencil-instructions stencil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miscellaneous

(defun program-buffer (program buffer-number)
  (do-program-buffers (buffer program)
    (when (= (buffer-number buffer) buffer-number)
      (return-from program-buffer buffer)))
  (error "No buffer with number ~D in program ~S."
         buffer-number
         program))

(defun program-kernel (program kernel-number)
  (do-program-kernels (kernel program)
    (when (= (kernel-number kernel) kernel-number)
      (return-from program-kernel kernel)))
  (error "No kernel with number ~D in program ~S."
         kernel-number
         program))

(declaim (inline count-mapped-elements))
(defun count-mapped-elements (map-fn what)
  (let ((counter 0))
    (declare (type (and fixnum unsigned-byte) counter))
    (funcall
     map-fn
     (lambda (element)
       (declare (ignore element))
       (incf counter))
     what)
    counter))

(defun buffer-number-of-inputs (buffer)
  (declare (buffer buffer))
  (count-mapped-elements #'map-buffer-inputs buffer))

(defun buffer-number-of-outputs (buffer)
  (declare (buffer buffer))
  (count-mapped-elements #'map-buffer-outputs buffer))

(defun buffer-number-of-loads (buffer)
  (declare (buffer buffer))
  (count-mapped-elements #'map-buffer-load-instructions buffer))

(defun buffer-number-of-stores (buffer)
  (declare (buffer buffer))
  (count-mapped-elements #'map-buffer-store-instructions buffer))

(defun kernel-number-of-inputs (kernel)
  (declare (kernel kernel))
  (count-mapped-elements #'map-kernel-inputs kernel))

(defun kernel-number-of-outputs (kernel)
  (declare (kernel kernel))
  (count-mapped-elements #'map-kernel-outputs kernel))

(defun kernel-number-of-loads (kernel)
  (declare (kernel kernel))
  (count-mapped-elements #'map-kernel-load-instructions kernel))

(defun kernel-number-of-stores (kernel)
  (declare (kernel kernel))
  (count-mapped-elements #'map-kernel-store-instructions kernel))

(defun kernel-highest-instruction-number (kernel)
  (declare (kernel kernel))
  (let ((max 0))
    ;; This function exploits that the numbers are handed out in
    ;; depth-first order, starting from the leaf instructions.  So we know
    ;; that the highest instruction number must be somewhere among the
    ;; store instructions.
    (map-kernel-store-instructions
     (lambda (store-instruction)
       (alexandria:maxf max (instruction-number store-instruction)))
     kernel)
    max))

;;; This function is a very ad-hoc approximation of the cost of executing
;;; the kernel.
(defun kernel-cost (kernel)
  (max 1 (* (shape-size (kernel-iteration-space kernel))
            (kernel-highest-instruction-number kernel))))

(defun make-buffer-like-array (buffer)
  (declare (buffer buffer))
  (make-array-from-shape-and-ntype
   (buffer-shape buffer)
   (buffer-ntype buffer)))

(defun make-array-from-shape-and-ntype (shape ntype)
  (declare (shape shape) (typo:ntype ntype))
  (make-array
   (shape-dimensions shape)
   :element-type (typo:ntype-type-specifier ntype)))

(defun ensure-array-buffer-compatibility (array buffer)
  (declare (array array) (buffer buffer))
  (ensure-array-shape-ntype-compatibility
   array
   (buffer-shape buffer)
   (buffer-ntype buffer)))

(defun ensure-array-shape-ntype-compatibility (array shape ntype)
  (declare (array array) (shape shape) (typo:ntype ntype))
  (unless (= (shape-rank shape) (array-rank array))
    (error "Expected an array of rank ~D, but got the rank ~D array~% ~S~%"
           (shape-rank shape) (array-rank array) array))
  (loop for range in (shape-ranges shape) for axis from 0 do
    (assert (= 0 (range-start range)))
    (assert (= 1 (range-step range)))
    (unless (= (array-dimension array axis) (range-size range))
      (error "Expected an array dimension of ~D in axis ~D, but got a dimension of ~D."
             (range-size range)
             axis
             (array-dimension array axis))))
  (unless (typo:ntype= (typo:array-element-ntype array)
                       (typo:upgraded-array-element-ntype ntype))
    (error "Not an array of type ~S: ~S"
           (array-element-type array)
           array))
  array)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IR Modifications

(defun delete-kernel (kernel)
  (map-kernel-inputs
   (lambda (buffer)
     (setf (buffer-readers buffer)
           (remove kernel (buffer-readers buffer) :key #'car)))
   kernel)
  (map-kernel-outputs
   (lambda (buffer)
     (setf (buffer-writers buffer)
           (remove kernel (buffer-writers buffer) :key #'car)))
   kernel)
  (setf (kernel-instruction-vector kernel)
        #())
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reasoning About Memory Locality
;;;
;;; If multiple consecutive loads or stores reference the same memory
;;; location, that memory reference can usually be served from a cache and
;;; is much faster.  Because data is eventually evicted from caches, it
;;; makes sense to rearrange buffers and kernel iteration spaces in such a
;;; way that the innermost loops of each kernel (i.e., the higher axes)
;;; have better data locality.
;;;
;;; The way we optimize for data locality is by introducing a notion of
;;; reuse potential.  Assuming a cache that is large enough to hold all
;;; elements referenced while traversing a particular axis of the iteration
;;; space, all loads with the same permutation and scalings but different
;;; offsets may lead to cache reuse.  We call this maximum number of
;;; attainable cache reuses the reuse potential.
;;;
;;; A reuse potential can be computed both for kernels and for buffers.  In
;;; both cases, we can then optimize the memory locality of the kernel or
;;; buffer by permuting its axes such that those with the hightes reuse
;;; potential appear last.

(defun kernel-reuse-potential (kernel)
  (let* ((rank (shape-rank (kernel-iteration-space kernel)))
         (result (make-array rank :initial-element 0)))
    (do-kernel-stencils (stencil kernel result)
      (dotimes (output-axis (stencil-output-rank stencil))
        (let ((input-axis (aref (stencil-output-mask stencil) output-axis))
              (test (differs-exactly-at output-axis))
              (alist '()))
          (unless (null input-axis)
            (dolist (instruction (stencil-instructions stencil))
              (let* ((transformation (instruction-transformation instruction))
                     (offsets (transformation-offsets transformation))
                     (entry (assoc offsets alist :test test)))
                (if (not entry)
                    (push (cons offsets 0) alist)
                    (incf (cdr entry)))))
            (loop for entry in alist do
              (incf (aref result input-axis)
                    (cdr entry)))))))))

(defun buffer-reuse-potential (buffer)
  (let* ((rank (shape-rank (buffer-shape buffer)))
         (result (make-array rank :initial-element 0)))
    (do-buffer-outputs (kernel buffer result)
      (dolist (stencil (kernel-load-stencils kernel buffer))
        (dotimes (output-axis rank)
          (let ((input-axis (aref (stencil-output-mask stencil) output-axis))
                (test (differs-exactly-at output-axis))
                (alist '()))
            (unless (null input-axis)
              (let ((size (range-size (shape-range (kernel-iteration-space kernel) input-axis))))
                (dolist (load-instruction (stencil-instructions stencil))
                  (let* ((transformation (load-instruction-transformation load-instruction))
                         (offsets (transformation-offsets transformation))
                         (entry (assoc offsets alist :test test)))
                    (if (not entry)
                        (push (cons offsets 0) alist)
                        (incf (cdr entry) size))))))
            (loop for entry in alist do
              (incf (aref result output-axis)
                    (cdr entry)))))))))

(defun differs-exactly-at (index)
  (lambda (a b)
    (let ((na (length a))
          (nb (length b)))
      (assert (= na nb))
      (loop for position below na
            for ea = (elt a position)
            for eb = (elt b position)
            always
            (if (= position index)
                (not (eql ea eb))
                (eql ea eb))))))

(defun reuse-optimizing-transformation (reuse-potential)
  "Takes a vector of single-precision floating-point numbers that describes
the potential for memory reuse along each axis, returns a transformation
that sorts all axes by increasing reuse potential."
  (make-transformation
   :output-mask
   (map 'vector #'car
        (stable-sort
         (loop for axis from 0 for rp across reuse-potential
               collect (cons axis rp))
         #'< :key #'cdr))))
