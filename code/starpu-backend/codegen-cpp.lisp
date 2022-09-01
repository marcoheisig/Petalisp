;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.starpu-backend)

(defun write-blueprint-cpp (name stream)
  (when *emit-verbose-code*
    (format stream "#include <stdio.h>~%"))
  (format stream "#include <math.h>~%")
  (format stream "#include <starpu.h>~%")
  (format stream "extern \"C\" {~%")
  (format stream "void ~A (void *buffers[], void *args) {~%" name)
  (when *emit-verbose-code*
    (format stream "  printf(\"Start executing ~A\\n\");~%" name))
  ;; Declare the iteration variables.
  (loop for axis below *iteration-space-rank* do
    (format stream "  int64_t start~D, end~D, step~D;~%" axis axis axis))
  ;; Declare the variables for each array data pointer and the
  ;; corresponding offsets and strides.
  (loop for (type rank) in *dst-array-info* for axis from 0 do
    (format stream "  ~A* __restrict dst~D;~@[ uint64_t ~{dst~Do~D~^, ~};~]~@[ uint64_t ~{dst~Ds~D~^, ~};~]~%"
            type axis
            (loop for index from 0 below rank
                  collect axis
                  collect index)
            (loop for index from 0 below (1- rank)
                  collect axis
                  collect index)))
  (loop for (type rank) in *src-array-info* for axis from 0 do
    (format stream "  ~A* __restrict src~D;~@[ uint64_t ~{src~Do~D~^, ~};~]~@[ uint64_t ~{src~Ds~D~^, ~};~]~%"
            type axis
            (loop for index from 0 below rank
                  collect axis
                  collect index)
            (loop for index from 0 below (1- rank)
                  collect axis
                  collect index)))
  ;; Unpack all StarPU arguments.
  (let ((arguments
          (append
           ;; Unpack the iteration space bounds.
           (loop for axis below *iteration-space-rank*
                 collect (format nil "start~D" axis)
                 collect (format nil "end~D" axis)
                 collect (format nil "step~D" axis))
           ;; Unpack the buffer offsets.
           ;; Unpack strides that couldn't be encoded as buffer dimensions.
           (loop for (nil rank) in *dst-array-info* for index from 0
                 append
                 (append
                  (loop for axis from 0 below rank
                        collect (format nil "dst~Do~D" index axis))
                  (loop for axis from 3 below (1- rank)
                        collect (format nil "dst~Ds~D" index axis))))
           (loop for (nil rank) in *src-array-info* for index from 0
                 append
                 (append
                  (loop for axis from 0 below rank
                        collect (format nil "src~Do~D" index axis))
                  (loop for axis from 3 below (1- rank)
                        collect (format nil "src~Ds~D" index axis)))))))
    (format stream "  starpu_codelet_unpack_args(args, ~{&~A~^, ~});~%"
            arguments)
    (when *emit-verbose-code*
      (loop for argument in arguments do
        (format stream "  printf(\"~A = %lli\\n\", ~:*~A);~%"
                argument))))
  ;; Unpack all StarPU buffers.
  (let ((buffer-number -1))
    (loop for (type rank) in *dst-array-info* for index from 0 do
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
    (loop for (type rank) in *src-array-info* for index from 0 do
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
  (loop for axis from 0 for range-info in *iteration-space-info* do
    (ecase range-info
      (:contiguous
       (format stream "  for (int64_t i~D = start~D; i~D < end~D; i~D += 1)~%"
               axis axis axis axis axis))
      (:strided
       (format stream "  for (int64_t i~D = start~D; i~D < end~D; i~D += step~D)~%"
               axis axis axis axis axis axis))))
  ;; Print the body.
  (format stream "  {~%")
  (when *emit-verbose-code*
    (format stream "    printf(\"iteration (~{~A~^ ~})\\n\"~{, i~D~});~%"
            (loop for axis from 0 for range-info in *iteration-space-info*
                  collect "%lli")
            (loop for axis from 0 for range-info in *iteration-space-info*
                  collect axis)))
  (let ((instruction-number -1))
    (ucons:do-ulist (instruction-blueprint *instruction-blueprint-ulist*)
      (write-instruction
       (format nil "v~D" (incf instruction-number))
       instruction-blueprint
       stream)))
  (format stream "  }~%")
  (when *emit-verbose-code*
    (format stream "  printf(\"Done executing ~A\\n\");~%" name)
    (format stream "  fflush(stdout);~%"))
  (format stream "}~%")
  (format stream "}~%"))
