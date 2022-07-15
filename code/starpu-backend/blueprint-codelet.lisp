;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.starpu-backend)

(defun blueprint-codelet (blueprint)
  (trivia:ematch blueprint
    ((ucons:ulist* _ target-array-info source-array-info)
     (starpu:make-codelet
      :name (princ-to-string blueprint)
      :modes (append (make-list (ucons:ulength target-array-info) :initial-element :w)
                     (make-list (ucons:ulength source-array-info) :initial-element :r))
      :cpu-func-0 (blueprint-cpu-func blueprint)
      :cpu-func-1 nil
      :cuda-func-0 nil))))

(defparameter *starpu-flags*
  (split-sequence:split-sequence
   #\space
   (uiop:run-program
    '("pkg-config" "--cflags" "--libs" "starpu-1.3")
    :output '(:string :stripped t))))

(defun compile-and-load-foreign-function (source-code &key (compiler "gcc"))
  (declare (string code))
  (uiop:with-temporary-file (:pathname library :type "so" :keep t)
    :close-stream
    (let* ((process-info (uiop:launch-program
                          (append
                           (list compiler "-x" "c++" "-std=c++14" "-O3" "-shared" "-pipe")
                           *starpu-flags*
                           (list "-o" (uiop:native-namestring library) "-"))
                          :input :stream
                          :error-output :stream))
           (input (uiop:process-info-input process-info))
           (error-output  (uiop:process-info-error-output process-info)))
      (unwind-protect (princ source-code input)
        (close input))
      (unless (zerop (uiop:wait-process process-info))
        (error "Error while compiling a shared C library:~%~A"
               (alexandria:read-stream-content-into-string error-output))))
    (cffi:load-foreign-library library)))

(defun blueprint-cpu-func (blueprint)
  (let ((name (format nil "petalisp_kernel_~X" (random most-positive-fixnum))))
    (cffi:foreign-symbol-pointer
     name
     :library
     (compile-and-load-foreign-function
      (with-output-to-string (stream)
        (write-blueprint-cpu-func name blueprint stream))))))

(defun write-blueprint-cpu-func (name blueprint stream)
  (trivia:ematch (ucons:tree-from-utree blueprint)
    ((list* iteration-space target-array-info source-array-info instruction-info)
     (format stream "#include <math.h>~%")
     (format stream "#include <starpu.h>~%")
     (format stream "extern \"C\" {~%")
     (format stream "void ~A (void *buffers[], void *args) {~%" name)
     ;; Declare the iteration variables.
     (loop for axis below (length iteration-space) do
       (format stream "  int64_t start~D, step~D, end~D;~%" axis axis axis))
     ;; Declare the variables for each array data pointer and the
     ;; corresponding offsets and strides.
     (loop for (ntype rank) in target-array-info for axis from 0 do
       (format stream "  ~A* __restrict dst~D;~@[ uint64_t ~{dst~Do~D~^, ~};~]~@[ uint64_t ~{dst~Ds~D~^, ~};~]~%"
               (ntype-c-type ntype)
               axis
               (loop for index from 0 below rank
                     collect axis
                     collect index)
               (loop for index from 0 below (1- rank)
                     collect axis
                     collect index)))
     (loop for (ntype rank) in source-array-info for axis from 0 do
       (format stream "  ~A* __restrict src~D;~@[ uint64_t ~{src~Do~D~^, ~};~]~@[ uint64_t ~{src~Ds~D~^, ~};~]~%"
               (ntype-c-type ntype)
               axis
               (loop for index from 0 below rank
                     collect axis
                     collect index)
               (loop for index from 0 below (1- rank)
                     collect axis
                     collect index)))
     ;; Unpack all StarPU arguments.
     (format stream "  starpu_codelet_unpack_args(args, ~{&~A~^, ~});~%"
             (append
              ;; Unpack the iteration space bounds.
              (loop for axis below (length iteration-space)
                    collect (format nil "start~D" axis)
                    collect (format nil "step~D" axis)
                    collect (format nil "end~D" axis))
              ;; Unpack the buffer offsets.
              ;; Unpack strides that couldn't be encoded as buffer dimensions.
              (loop for (nil rank) in target-array-info for index from 0
                    append
                    (append
                     (loop for axis from 0 below rank
                           collect (format nil "dst~Do~D" index axis))
                     (loop for axis from 3 below (1- rank)
                           collect (format nil "dst~Ds~D" index axis))))
              (loop for (nil rank) in source-array-info for index from 0
                    append
                    (append
                     (loop for axis from 0 below rank
                           collect (format nil "src~Do~D" index axis))
                     (loop for axis from 3 below (1- rank)
                           collect (format nil "src~Ds~D" index axis))))))
     ;; Unpack all StarPU buffers.
     (let ((buffer-number -1))
       (loop
         for (ntype rank) in target-array-info
         for index from 0
         for type = (ntype-c-type ntype) do
           (incf buffer-number)
           (case rank
             ((0 1)
              (format stream "  dst~D = (~A*)STARPU_VECTOR_GET_PTR(buffers[~D]);~%"
                      index type buffer-number))
             (2
              (format stream "  dst~D = (~A*)STARPU_MATRIX_GET_PTR(buffers[~D]);~%"
                      index type buffer-number)
              (format stream "  dst~Ds0 = STARPU_MATRIX_GET_NY(buffers[~D]);~%"
                      index buffer-number))
             (otherwise
              (format stream "  dst~D = (~A*)STARPU_BLOCK_GET_PTR(buffers[~D]);~%"
                      index type buffer-number)
              (format stream "  dst~Ds1 = STARPU_BLOCK_GET_NZ(buffers[~D])~@[ * dst~Ds2~];~%"
                      index buffer-number (and (> rank 3) index))
              (format stream "  dst~Ds0 = STARPU_BLOCK_GET_NY(buffers[~D]) * dst~Ds1;~%"
                      index buffer-number index))))
       (loop
         for (ntype rank) in source-array-info
         for index from 0
         for type = (ntype-c-type ntype) do
           (incf buffer-number)
           (case rank
             ((0 1)
              (format stream "  src~D = (~A*)STARPU_VECTOR_GET_PTR(buffers[~D]);~%"
                      index type buffer-number))
             (2
              (format stream "  src~D = (~A*)STARPU_MATRIX_GET_PTR(buffers[~D]);~%"
                      index type buffer-number)
              (format stream "  src~Ds0 = STARPU_MATRIX_GET_NY(buffers[~D]);~%"
                      index buffer-number))
             (otherwise
              (format stream "  src~D = (~A*)STARPU_BLOCK_GET_PTR(buffers[~D]);~%"
                      index type buffer-number)
              (format stream "  src~Ds1 = STARPU_BLOCK_GET_NZ(buffers[~D])~@[ * src~Ds2~];~%"
                      index buffer-number (and (> rank 3) index))
              (format stream "  src~Ds0 = STARPU_BLOCK_GET_NY(buffers[~D]) * src~Ds1;~%"
                      index buffer-number index)))))
     ;; Loop over the iteration space.
     (loop for axis from 0 for range-info in iteration-space do
       (ecase range-info
         (:contiguous
          (format stream "  for (int i~D = start~D; i~D < step~D; i~D += 1)~%"
                  axis axis axis axis axis))
         (:strided
          (format stream "  for (int i~D = start~D; i~D < step~D; i~D += step~D)~%"
                  axis axis axis axis axis axis))))
     ;; Print the body.
     (format stream "  {~%")
     (loop for instruction in instruction-info for instruction-number from 0 do
       (trivia:match instruction
         ((list* :call number-of-values operator inputs)
          (unless (= 1 number-of-values)
            (error "Cannot create C++ kernels containing multiple valued functions."))
          (multiple-value-bind (type name kind)
              (decode-operator operator)
            (let ((input-numbers (mapcar #'second inputs)))
              (ecase kind
                (:infix
                 (assert (= 2 (length inputs)))
                 (format stream "    ~A v~D = v~D ~A v~D;~%"
                         type
                         instruction-number
                         (first input-numbers)
                         name
                         (second input-numbers)))
                (:prefix
                 (format stream "    ~A v~D = ~A(~{v~D~^, ~});~%"
                         type
                         instruction-number
                         name
                         input-numbers))))))
         ((list* :load buffer-number irefs)
          (format stream "    ~A v~D = src~D["
                  (ntype-c-type (first (nth buffer-number source-array-info)))
                  instruction-number
                  buffer-number)
          (loop for (iref rest) on irefs for axis from 0 do
            (format stream "(")
            (write-iref iref stream)
            (if (null rest)
                (format stream " - src~Do~D)"
                        buffer-number axis)
                (format stream " * src~Ds~d - src~Do~D) + "
                        buffer-number axis buffer-number axis)))
          (when (null irefs) (format stream "0"))
          (format stream "];~%"))
         ((list* :store input buffer-number irefs)
          (format stream "    dst~D[" buffer-number)
          (loop for (iref rest) on irefs for axis from 0 do
            (format stream "(")
            (write-iref iref stream)
            (if (null rest)
                (format stream " - dst~Do~D)"
                        buffer-number axis)
                (format stream " * dst~Ds~d - dst~Do~D) + "
                        buffer-number axis buffer-number axis)))
          (when (null irefs) (format stream "0"))
          (format stream "] = v~D;~%" (second input)))
         ((list :iref iref)
          (format stream "    v~D = " instruction-number)
          (write-iref iref stream)
          (format stream ";~%"))))
     (format stream "  }~%")
     (format stream "}~%")
     (format stream "}~%"))))

(defun write-iref (iref stream)
  (destructuring-bind (permutation scaling offset) iref
    (cond ((= scaling 0)
           (format stream "~D" offset))
          ((and (= offset 0) (= scaling 1))
           (format stream "i~D" permutation))
          ((= offset 0)
           (format stream "(i~D * ~D)" permutation scaling))
          (t
           (format stream "(i~D * ~D + ~D)" permutation scaling offset)))))

(defun ntype-c-type (ntype)
  (petalisp.type-inference:ntype-subtypecase ntype
    (single-float "float")
    (double-float "double")
    ((unsigned-byte 8) "uint8_t")
    ((unsigned-byte 16) "uint16_t")
    ((unsigned-byte 32) "uint32_t")
    ((unsigned-byte 64) "uint64_t")
    ((signed-byte 8) "int8_t")
    ((signed-byte 16) "int16_t")
    ((signed-byte 32) "int32_t")
    ((signed-byte 64) "int64_t")
    (t (error "Cannot create C++ kernels operating on values of type ~S."
              (petalisp.type-inference:type-specifier ntype)))))

(defparameter *operator-table*
  (alexandria:alist-hash-table
   '(;; coerce
     (petalisp.type-inference::coerce-to-short-float "float" "(float)" :prefix)
     (petalisp.type-inference::coerce-to-single-float "float" "(float)" :prefix)
     (petalisp.type-inference::coerce-to-double-float "double" "(double)" :prefix)
     (petalisp.type-inference::coerce-to-long-float "double" "(double)" :prefix)
     ;; short-float
     (petalisp.type-inference::short-float+ "float" "+" :infix)
     (petalisp.type-inference::short-float- "float" "-" :infix)
     (petalisp.type-inference::short-float* "float" "*" :infix)
     (petalisp.type-inference::short-float/ "float" "/" :infix)
     (petalisp.type-inference::short-float-abs "float" "fabs" :prefix)
     (petalisp.type-inference::short-float-from-short-float "float" "(float)" :prefix)
     (petalisp.type-inference::short-float-from-double-float "float" "(float)" :prefix)
     (petalisp.type-inference::short-float-from-long-float "float" "(float)" :prefix)
     (petalisp.type-inference::short-float-cos "float" "cos" :prefix)
     (petalisp.type-inference::short-float-exp "float" "exp" :prefix)
     (petalisp.type-inference::short-float-ln "float" "ln" :prefix)
     (petalisp.type-inference::short-float-max "float" "fmax" :prefix)
     (petalisp.type-inference::short-float-min "float" "fmin" :prefix)
     (petalisp.type-inference::short-float-sin "float" "sin" :prefix)
     (petalisp.type-inference::short-float-sqrt "float" "sqrt" :prefix)
     (petalisp.type-inference::short-float-tan "float" "tan" :prefix)
     (petalisp.type-inference::short-float-unary- "float" "-" :prefix)
     ;; single-float
     (petalisp.type-inference::single-float+ "float" "+" :infix)
     (petalisp.type-inference::single-float- "float" "-" :infix)
     (petalisp.type-inference::single-float* "float" "*" :infix)
     (petalisp.type-inference::single-float/ "float" "/" :infix)
     (petalisp.type-inference::single-float-abs "float" "fabs" :prefix)
     (petalisp.type-inference::single-float-from-short-float "float" "(float)" :prefix)
     (petalisp.type-inference::single-float-from-double-float "float" "(float)" :prefix)
     (petalisp.type-inference::single-float-from-long-float "float" "(float)" :prefix)
     (petalisp.type-inference::single-float-cos "float" "cos" :prefix)
     (petalisp.type-inference::single-float-exp "float" "exp" :prefix)
     (petalisp.type-inference::single-float-ln "float" "ln" :prefix)
     (petalisp.type-inference::single-float-max "float" "fmax" :prefix)
     (petalisp.type-inference::single-float-min "float" "fmin" :prefix)
     (petalisp.type-inference::single-float-sin "float" "sin" :prefix)
     (petalisp.type-inference::single-float-sqrt "float" "sqrt" :prefix)
     (petalisp.type-inference::single-float-tan "float" "tan" :prefix)
     (petalisp.type-inference::single-float-unary- "float" "-" :prefix)
     ;; double-float
     (petalisp.type-inference::double-float+ "double" "+" :infix)
     (petalisp.type-inference::double-float- "double" "-" :infix)
     (petalisp.type-inference::double-float* "double" "*" :infix)
     (petalisp.type-inference::double-float/ "double" "/" :infix)
     (petalisp.type-inference::double-float-abs "double" "fabs" :prefix)
     (petalisp.type-inference::double-float-from-short-float "double" "(double)" :prefix)
     (petalisp.type-inference::double-float-from-single-float "double" "(double)" :prefix)
     (petalisp.type-inference::double-float-from-long-float "double" "(double)" :prefix)
     (petalisp.type-inference::double-float-cos "double" "cos" :prefix)
     (petalisp.type-inference::double-float-exp "double" "exp" :prefix)
     (petalisp.type-inference::double-float-ln "double" "ln" :prefix)
     (petalisp.type-inference::double-float-max "double" "fmax" :prefix)
     (petalisp.type-inference::double-float-min "double" "fmin" :prefix)
     (petalisp.type-inference::double-float-sin "double" "sin" :prefix)
     (petalisp.type-inference::double-float-sqrt "double" "sqrt" :prefix)
     (petalisp.type-inference::double-float-tan "double" "tan" :prefix)
     (petalisp.type-inference::double-float-unary- "double" "-" :prefix)
     ;; long-float
     (petalisp.type-inference::long-float+ "double" "+" :infix)
     (petalisp.type-inference::long-float- "double" "-" :infix)
     (petalisp.type-inference::long-float* "double" "*" :infix)
     (petalisp.type-inference::long-float/ "double" "/" :infix)
     (petalisp.type-inference::long-float-abs "double" "fabs" :prefix)
     (petalisp.type-inference::long-float-from-short-float "double" "(double)" :prefix)
     (petalisp.type-inference::long-float-from-single-float "double" "(double)" :prefix)
     (petalisp.type-inference::long-float-from-long-float "double" "(double)" :prefix)
     (petalisp.type-inference::long-float-cos "double" "cos" :prefix)
     (petalisp.type-inference::long-float-exp "double" "exp" :prefix)
     (petalisp.type-inference::long-float-ln "double" "ln" :prefix)
     (petalisp.type-inference::long-float-max "double" "fmax" :prefix)
     (petalisp.type-inference::long-float-min "double" "fmin" :prefix)
     (petalisp.type-inference::long-float-sin "double" "sin" :prefix)
     (petalisp.type-inference::long-float-sqrt "double" "sqrt" :prefix)
     (petalisp.type-inference::long-float-tan "double" "tan" :prefix)
     (petalisp.type-inference::long-float-unary- "double" "-" :prefix))))

(defun decode-operator (operator)
  (values-list
   (or (gethash operator *operator-table*)
       (error "Cannot create C++ kernels containing ~S operators." operator))))
