;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.codegen)

;;; Code for converting Petalisp blueprints to C++ code.  The variable naming
;;; convention is as follows:
;;;
;;; startN, endN, stepN are the iteration space parameters of the Nth axis.
;;;
;;; iN is the iteration variable of the Nth loop.
;;;
;;; oN is the Nth offset parameter appearing in the kernel.
;;;
;;; sN is the Nth scaling parameter appearing in the kernel.
;;;
;;; dstK is the data pointer of the Kth target array.
;;;
;;; dstKskip is the position of the first element of the Kth target array
;;; that was actually allocated.
;;;
;;; dstKsI is the stride of the Ith axis of the Kth destination array.
;;;
;;; dstKiL is the index of the Lth reference into Kth destination array.
;;;
;;; The variables srcK, srcKskip, srcKsI, and srcKiT are analogously to
;;; their dst counterparts, but for source arrays.

;;; The rank of the iteration space of the current blueprint.
(defvar *iteration-space-rank*)

;;; A list with one entry per iteration space axis, whose entries are
;;; either :CONTIGUOUS or :STRIDED.
(defvar *iteration-space-info*)

;;; (NTYPE . TRANSFORMATIONS) entries where the Kth entry describes the
;;; type and all references into the Kth target array.
(defvar *dst-array-info*)

;;; (NTYPE . TRANSFORMATIONS) entries where the Kth entry describes the
;;; type and all references into the Kth source array.
(defvar *src-array-info*)

;;; An list of instruction blueprints (converted from ulists to lists).
(defvar *instruction-blueprints*)

;;; A boolean, indicating whether the generated code should print all its
;;; operations to stdout.
(defparameter *emit-verbose-code* nil)

(defmacro with-blueprint-info ((blueprint) &body body)
  `(call-with-blueprint-info ,blueprint (lambda () ,@body)))

(defun call-with-blueprint-info (blueprint thunk)
  (trivia:ematch (ucons:tree-from-utree blueprint)
    ((list* iteration-space targets sources instructions)
     (let* ((*iteration-space-info* iteration-space)
            (*iteration-space-rank* (length iteration-space))
            (*dst-array-info* targets)
            (*src-array-info* sources)
            (*instruction-blueprints* instructions))
       (funcall thunk)))))

(defvar *scaling-counter*)

(defvar *offset-counter*)

(defvar *index-counter*)

(defun write-body (stream)
  (format stream "  {~%")
  (when *emit-verbose-code*
    (format stream "    printf(\"iteration (~{~A~^ ~})\\n\"~{, i~D~});~%"
            (loop for axis below *iteration-space-rank* collect "%lld")
            (loop for axis below *iteration-space-rank* collect axis)))
  (let ((instruction-number 0)
        (*scaling-counter* 0)
        (*offset-counter* 0)
        (*index-counter* 0))
    (loop for (nil . refs) in *dst-array-info* for k from 0 do
      (loop for irefs in refs for l from 0 do
        (format stream "    int64_t dst~Di~D = " k l)
        (write-irefs irefs (format nil "dst~D" k) stream)
        (format stream ";~%")))
    (loop for (nil . refs) in *src-array-info* for k from 0 do
      (loop for irefs in refs for l from 0 do
        (format stream "    int64_t src~Di~D = " k l)
        (write-irefs irefs (format nil "src~D" k) stream)
        (format stream ";~%")))
    (dolist (instruction-blueprint *instruction-blueprints*)
      (write-instruction
       (format nil "v~D" instruction-number)
       instruction-blueprint
       stream)
      (incf instruction-number)))
  (format stream "  }~%"))

(defun write-instruction (target-variable instruction stream)
  (trivia:ematch instruction
    ((list* :call number-of-values operator inputs)
     (unless (= 1 number-of-values)
       (error "Cannot (yet) create C++ kernels containing multiple valued functions."))
     (multiple-value-bind (type name kind)
         (decode-operator operator)
       (let ((input-numbers (mapcar #'second inputs)))
         (ecase kind
           (:infix
            (assert (= 2 (length inputs)))
            (format stream "    ~A ~A = v~D ~A v~D;~%"
                    type
                    target-variable
                    (first input-numbers)
                    name
                    (second input-numbers)))
           (:prefix
            (format stream "    ~A ~A = ~A(~{v~D~^, ~});~%"
                    type
                    target-variable
                    name
                    input-numbers)))
         (when *emit-verbose-code*
           (format stream "    printf(\"~A = ~A\\n\", ~A);~%"
                   target-variable
                   (type-format-string type)
                   target-variable)))))
    ((list* :load buffer-number ref-number offsets)
     (format stream "    int64_t index~D = src~Di~D"
             *index-counter* buffer-number ref-number)
     (loop for (offset . more-offsets) on offsets for axis from 0 do
       (unless (zerop offset)
         (if (null more-offsets)
             (format stream " + ~D" offset)
             (format stream " + src~Ds~D * ~D" buffer-number axis offset))))
     (format stream ";~%")
     (format stream "    ~A ~A = src~D[index~D];~%"
             (ntype-c-type (first (nth buffer-number *src-array-info*)))
             target-variable
             buffer-number
             *index-counter*)
     (when *emit-verbose-code*
       (format stream "    printf(\"~A = src~D[%lld] = ~A\\n\", index~D, ~A);~%"
               target-variable
               buffer-number
               (type-format-string
                (ntype-c-type
                 (first (nth buffer-number *src-array-info*))))
               *index-counter*
               target-variable))
     (incf *index-counter*))
    ((list :store (list _ instruction-number) buffer-number ref-number)
     (format stream "    int64_t index~D = dst~Di~D;~%"
             *index-counter* buffer-number ref-number)
     (format stream "    dst~D[index~D] = v~D;~%"
             buffer-number
             *index-counter*
             instruction-number)
     (when *emit-verbose-code*
       (format stream "    printf(\"dst~D[%lld] = v~D\\n\", index~D);~%"
               buffer-number
               instruction-number
               *index-counter*))
     (incf *index-counter*))
    ((list :iref iref)
     (format stream "    int64_t ~A = " target-variable)
     (write-iref iref stream)
     (format stream ";~%"))))

(defun write-irefs (irefs prefix stream)
  (if (null irefs)
      (format stream "0")
      (progn
        (format stream "(")
        (loop for (iref rest) on irefs for axis from 0 do
          (write-iref iref stream)
          (if (null rest)
              (format stream " - ~Askip" prefix)
              (format stream " * ~As~d + " prefix axis)))
        (format stream ")"))))

(defun write-iref (iref stream)
  (let ((offset (format nil "o~D" *offset-counter*)))
    (incf *offset-counter*)
    (trivia:ematch iref
      ((list _ 0)
       (format stream "~D" offset))
      ((list permutation 1)
       (format stream "(i~D + ~D)" permutation offset))
      ((list permutation (and scaling (type integer)))
       (format stream "(i~D * ~A + ~A)" permutation scaling offset))
      ((list permutation :any)
       (let ((scaling (format nil "s~D" *scaling-counter*)))
         (incf *scaling-counter*)
         (format stream "(i~D * ~A + ~A)" permutation scaling offset))))))

(defun write-prologue (name stream)
  (when *emit-verbose-code*
    (format stream "  printf(\"Start executing ~A\\n\");~%" name))
  ;; Declare the iteration variables.
  (loop for axis below *iteration-space-rank* do
    (format stream "  int64_t start~D, end~D, step~D;~%" axis axis axis))
  ;; Declare the variables for each array data pointer and the
  ;; corresponding skips and strides.
  (loop for (ntype ref) in *dst-array-info* for axis from 0 do
    (let ((type (ntype-c-type ntype))
          (rank (length ref)))
      (format stream "  ~A* __restrict dst~D; uint64_t dst~Dskip;~@[ uint64_t ~{dst~Ds~D~^, ~};~]~%"
              type axis axis
              (loop for index from 0 below (1- rank)
                    collect axis
                    collect index))))
  (loop for (ntype ref) in *src-array-info* for axis from 0 do
    (let ((type (ntype-c-type ntype))
          (rank (length ref)))
      (format stream "  ~A* __restrict src~D; uint64_t src~Dskip;~@[ uint64_t ~{src~Ds~D~^, ~};~]~%"
              type axis axis
              (loop for index from 0 below (1- rank)
                    collect axis
                    collect index))))
  (let* ((coeffs (kernel-coeffs))
         (arguments
           (append
            ;; Collect the iteration space bounds.
            (loop for axis below *iteration-space-rank*
                  collect (format nil "start~D" axis)
                  collect (format nil "end~D" axis)
                  collect (format nil "step~D" axis))
            ;; Collect strides that couldn't be encoded as buffer
            ;; properties, and skips
            (loop for (nil ref) in *dst-array-info* for index from 0
                  collect (format nil "dst~Dskip" index)
                  append
                  (loop for axis from 3 below (1- (length ref))
                        collect (format nil "dst~Ds~D" index axis)))
            (loop for (nil ref) in *src-array-info* for index from 0
                  collect (format nil "src~Dskip" index)
                  append
                  (loop for axis from 3 below (1- (length ref))
                        collect (format nil "src~Ds~D" index axis)))
            coeffs)))
    ;; Declare variables for the remaining coefficients.
    (unless (null coeffs)
      (format stream "  int64_t~{ ~A~^,~};~%" coeffs))
    ;; Unpack all StarPU arguments.
    (unless (null arguments)
      (format stream "  starpu_codelet_unpack_args(args~{, &~A~});~%"
              arguments)
      (when *emit-verbose-code*
        (loop for argument in arguments do
          (format stream "  printf(\"~A = %lld\\n\", ~:*~A);~%"
                  argument)))))
  ;; Unpack all StarPU buffers.
  (let ((buffer-number -1))
    (loop for (ntype ref) in *dst-array-info* for index from 0 do
      (let ((type (ntype-c-type ntype))
            (rank (length ref)))
        (incf buffer-number)
        (case rank
          ((0 1)
           (format stream "  dst~D = (~A*)STARPU_VECTOR_GET_PTR(buffers[~D]);~%"
                   index type buffer-number))
          (2
           (format stream "  dst~D = (~A*)STARPU_MATRIX_GET_PTR(buffers[~D]);~%"
                   index type buffer-number)
           (format stream "  dst~Ds0 = STARPU_MATRIX_GET_LD(buffers[~D]);~%"
                   index buffer-number))
          (otherwise
           (format stream "  dst~D = (~A*)STARPU_BLOCK_GET_PTR(buffers[~D]);~%"
                   index type buffer-number)
           (format stream "  dst~Ds1 = STARPU_BLOCK_GET_LDY(buffers[~D]);~%"
                   index buffer-number)
           (format stream "  dst~Ds0 = STARPU_BLOCK_GET_LDZ(buffers[~D]);~%"
                   index buffer-number)))))
    (loop for (ntype ref) in *src-array-info* for index from 0 do
      (let ((type (ntype-c-type ntype))
            (rank (length ref)))
        (incf buffer-number)
        (case rank
          ((0 1)
           (format stream "  src~D = (~A*)STARPU_VECTOR_GET_PTR(buffers[~D]);~%"
                   index type buffer-number))
          (2
           (format stream "  src~D = (~A*)STARPU_MATRIX_GET_PTR(buffers[~D]);~%"
                   index type buffer-number)
           (format stream "  src~Ds0 = STARPU_MATRIX_GET_LD(buffers[~D]);~%"
                   index buffer-number))
          (otherwise
           (format stream "  src~D = (~A*)STARPU_BLOCK_GET_PTR(buffers[~D]);~%"
                   index type buffer-number)
           (format stream "  src~Ds1 = STARPU_BLOCK_GET_LDY(buffers[~D]);~%"
                   index buffer-number)
           (format stream "  src~Ds0 = STARPU_BLOCK_GET_LDZ(buffers[~D]);~%"
                   index buffer-number)))))))

(defun kernel-coeffs ()
  (let ((scaling-counter 0)
        (offset-counter 0)
        (coeffs '()))
    (flet ((process-iref (iref)
             (declare (ignore iref))
             (push (format nil "s~D" scaling-counter) coeffs)
             (incf scaling-counter)
             (push (format nil "o~D" offset-counter) coeffs)
             (incf offset-counter)))
      (loop for (nil . refs) in *dst-array-info* do
        (dolist (irefs refs)
          (mapc #'process-iref irefs)))
      (loop for (nil . refs) in *src-array-info* do
        (dolist (irefs refs)
          (mapc #'process-iref irefs)))
      (dolist (instruction-blueprint *instruction-blueprints*)
        (trivia:match instruction-blueprint
          ((list :iref iref)
           (process-iref iref)))))
    (nreverse coeffs)))

(defun write-epilogue (name stream)
  (when *emit-verbose-code*
    (format stream "  printf(\"Done executing ~A\\n\");~%" name)
    (format stream "  fflush(NULL);~%")))

(defun ntype-c-type (ntype)
  (typo:ntype-subtypecase ntype
    (short-float "float")
    (single-float "float")
    (double-float "double")
    (long-float "double")
    ((unsigned-byte 8) "uint8_t")
    ((unsigned-byte 16) "uint16_t")
    ((unsigned-byte 32) "uint32_t")
    ((unsigned-byte 64) "uint64_t")
    ((signed-byte 8) "int8_t")
    ((signed-byte 16) "int16_t")
    ((signed-byte 32) "int32_t")
    ((signed-byte 64) "int64_t")
    (t (error "Cannot create C++ kernels operating on values of type ~S."
              (typo:ntype-type-specifier ntype)))))

(defun type-format-string (type)
  (trivia:ematch type
    ("float" "%g")
    ("double" "%g")))

(defparameter *operator-table*
  (alexandria:alist-hash-table
   '(;; coerce
     (typo:coerce-to-short-float "float" "(float)" :prefix)
     (typo:coerce-to-single-float "float" "(float)" :prefix)
     (typo:coerce-to-double-float "double" "(double)" :prefix)
     (typo:coerce-to-long-float "double" "(double)" :prefix)
     ;; short-float
     (typo:two-arg-short-float+ "float" "+" :infix)
     (typo:two-arg-short-float- "float" "-" :infix)
     (typo:two-arg-short-float* "float" "*" :infix)
     (typo:two-arg-short-float/ "float" "/" :infix)
     (typo:two-arg-short-float-max "float" "fmax" :prefix)
     (typo:two-arg-short-float-min "float" "fmin" :prefix)
     (typo:short-float-abs "float" "fabs" :prefix)
     (typo:short-float-from-single-float "float" "(float)" :prefix)
     (typo:short-float-from-double-float "float" "(float)" :prefix)
     (typo:short-float-from-long-float "float" "(float)" :prefix)
     (typo:short-float-cos "float" "cos" :prefix)
     (typo:short-float-exp "float" "exp" :prefix)
     (typo:short-float-ln "float" "ln" :prefix)
     (typo:short-float-sin "float" "sin" :prefix)
     (typo:short-float-sqrt "float" "sqrt" :prefix)
     (typo:short-float-tan "float" "tan" :prefix)
     (typo:one-arg-short-float- "float" "-" :prefix)
     ;; single-float
     (typo:two-arg-single-float+ "float" "+" :infix)
     (typo:two-arg-single-float- "float" "-" :infix)
     (typo:two-arg-single-float* "float" "*" :infix)
     (typo:two-arg-single-float/ "float" "/" :infix)
     (typo:two-arg-single-float-max "float" "fmax" :prefix)
     (typo:two-arg-single-float-min "float" "fmin" :prefix)
     (typo:single-float-abs "float" "fabs" :prefix)
     (typo:single-float-from-short-float "float" "(float)" :prefix)
     (typo:single-float-from-double-float "float" "(float)" :prefix)
     (typo:single-float-from-long-float "float" "(float)" :prefix)
     (typo:single-float-cos "float" "cos" :prefix)
     (typo:single-float-exp "float" "exp" :prefix)
     (typo:single-float-ln "float" "ln" :prefix)
     (typo:single-float-sin "float" "sin" :prefix)
     (typo:single-float-sqrt "float" "sqrt" :prefix)
     (typo:single-float-tan "float" "tan" :prefix)
     (typo:one-arg-single-float- "float" "-" :prefix)
     ;; double-float
     (typo:two-arg-double-float+ "double" "+" :infix)
     (typo:two-arg-double-float- "double" "-" :infix)
     (typo:two-arg-double-float* "double" "*" :infix)
     (typo:two-arg-double-float/ "double" "/" :infix)
     (typo:two-arg-double-float-max "double" "fmax" :prefix)
     (typo:two-arg-double-float-min "double" "fmin" :prefix)
     (typo:double-float-abs "double" "fabs" :prefix)
     (typo:double-float-from-short-float "double" "(double)" :prefix)
     (typo:double-float-from-single-float "double" "(double)" :prefix)
     (typo:double-float-from-long-float "double" "(double)" :prefix)
     (typo:double-float-cos "double" "cos" :prefix)
     (typo:double-float-exp "double" "exp" :prefix)
     (typo:double-float-ln "double" "ln" :prefix)
     (typo:double-float-sin "double" "sin" :prefix)
     (typo:double-float-sqrt "double" "sqrt" :prefix)
     (typo:double-float-tan "double" "tan" :prefix)
     (typo:one-arg-double-float- "double" "-" :prefix)
     ;; long-float
     (typo:two-arg-long-float+ "double" "+" :infix)
     (typo:two-arg-long-float- "double" "-" :infix)
     (typo:two-arg-long-float* "double" "*" :infix)
     (typo:two-arg-long-float/ "double" "/" :infix)
     (typo:two-arg-long-float-max "double" "fmax" :prefix)
     (typo:two-arg-long-float-min "double" "fmin" :prefix)
     (typo:long-float-abs "double" "fabs" :prefix)
     (typo:long-float-from-short-float "double" "(double)" :prefix)
     (typo:long-float-from-single-float "double" "(double)" :prefix)
     (typo:long-float-from-double-float "double" "(double)" :prefix)
     (typo:long-float-cos "double" "cos" :prefix)
     (typo:long-float-exp "double" "exp" :prefix)
     (typo:long-float-ln "double" "ln" :prefix)
     (typo:long-float-sin "double" "sin" :prefix)
     (typo:long-float-sqrt "double" "sqrt" :prefix)
     (typo:long-float-tan "double" "tan" :prefix)
     (typo:one-arg-long-float- "double" "-" :prefix))))

(defun blueprint-cpp-translatable-p (blueprint)
  (ucons:do-ulist (instruction-blueprint (ucons:unthcdr 3 blueprint) t)
    (trivia:match instruction-blueprint
      ((ucons:ulist* :call 1 function-name _)
       (when (or (eq function-name :any)
                 (not (gethash function-name *operator-table*)))
         (return-from blueprint-cpp-translatable-p nil))))))

(defun decode-operator (operator)
  (values-list
   (or (gethash operator *operator-table*)
       (error "Cannot create C++ kernels containing ~S operators." operator))))

(defun pkg-config (lib-name &rest options)
  (values
   (split-sequence:split-sequence
    #\space
    (uiop:run-program
     (list* "pkg-config" lib-name options)
     :output '(:string :stripped t)))))

(defun load-foreign-code
    (source-code
     &key
       (compiler "cc")
       (language "c++")
       (standard "c++14")
       (flags '()))
  (declare (string source-code compiler language standard)
           (list flags))
  (uiop:with-temporary-file (:pathname library :type "so" :keep t)
    :close-stream
    (let* ((command
             (append
              (list compiler "-shared" "-x" language (format nil "-std=~A" standard))
              flags
              (list "-o" (uiop:native-namestring library) "-")))
           (process-info (uiop:launch-program
                          command
                          :input :stream
                          :error-output :stream))
           (input (uiop:process-info-input process-info))
           (error-output  (uiop:process-info-error-output process-info)))
      (unwind-protect (princ source-code input)
        (close input))
      (unless (zerop (uiop:wait-process process-info))
        (error "Error while compiling a shared library:~%~A~%~A"
               (with-output-to-string (stream)
                 (loop for string in command do
                   (write-string string stream)
                   (write-char #\space stream)))
               (alexandria:read-stream-content-into-string error-output))))
    (cffi:load-foreign-library library)))

(defun write-blueprint-cpp (name stream)
  (when *emit-verbose-code*
    (format stream "#include <stdio.h>~%"))
  (format stream "#include <math.h>~%")
  (format stream "#include <starpu.h>~%")
  (format stream "extern \"C\" {~%")
  (format stream "void ~A (void *buffers[], void *args) {~%" name)
  (write-prologue name stream)
  ;; Loop over the iteration space.
  (loop for axis from 0 for range-info in *iteration-space-info* do
    (ecase range-info
      (:contiguous
       (format stream "  for (int64_t i~D = start~D; i~D < end~D; i~D += 1)~%"
               axis axis axis axis axis))
      (:strided
       (format stream "  for (int64_t i~D = start~D; i~D < end~D; i~D += step~D)~%"
               axis axis axis axis axis axis))))
  ;; Print the body.
  (write-body stream)
  (write-epilogue name stream)
  (format stream "}~%")
  (format stream "}~%"))
