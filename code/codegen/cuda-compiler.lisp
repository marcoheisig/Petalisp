;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.codegen)

(defun write-blueprint-cuda (name stream)
  (format stream "#include <math.h>~%")
  (format stream "#include <starpu.h>~%")
  ;; Write the CUDA kernel.
  (format stream "static __global__ void ~A_impl(~{~A~^, ~}) {~%"
          name
          (append
           (loop for axis below *iteration-space-rank*
                 collect (format nil "int64_t start~D" axis)
                 collect (format nil "int64_t end~D" axis)
                 collect (format nil "int64_t step~D" axis))
           (loop for (ntype irefs) in *dst-array-info* for axis from 0
                 for type = (ntype-c-type ntype)
                 for rank = (length irefs)
                 collect (format nil "~A* __restrict dst~D" type axis)
                 collect (format nil "uint64_t dst~Dskip" axis)
                 append (loop for index below (1- rank) collect (format nil "uint64_t dst~Ds~D" axis index)))
           (loop for (ntype irefs) in *src-array-info* for axis from 0
                 for type = (ntype-c-type ntype)
                 for rank = (length irefs)
                 collect (format nil "const ~A* __restrict src~D" type axis)
                 collect (format nil "uint64_t src~Dskip" axis)
                 append (loop for index below (1- rank) collect (format nil "uint64_t src~Ds~D" axis index)))
           (loop for coeff in (kernel-coeffs)
                 collect (format nil "int64_t ~A" coeff))))
  ;; Compute the loop indices.
  (loop for range-info in *iteration-space-info* for axis from 0 do
    (if (< axis 3)
        (let ((char (schar "xyz" (- *iteration-space-rank* 1 axis))))
          (when *emit-verbose-code*
            (format stream "  printf(\"blockIdx.~C: %d, blockDim.~C: %d, threadIdx.~C: %d\\n\", blockIdx.~C, blockDim.~C, threadIdx.~C);~%"
                    char char char char char char))
          (format stream "  int64_t i~D = start~D + (blockIdx.~C * blockDim.~C + threadIdx.~C) * step~D;~%"
                  axis axis char char char axis)
          (format stream "  if (!(i~D < end~D)) {return;}~%"
                  axis axis))
        (ecase range-info
          (:contiguous
           (format stream "  for (int64_t i~D = start~D; i~D < end~D; i~D += 1)~%"
                   axis axis axis axis axis))
          (:strided
           (format stream "  for (int64_t i~D = start~D; i~D < end~D; i~D += step~D)~%"
                   axis axis axis axis axis axis)))))
  ;; Write the CUDA kernel's body.
  (write-body stream)
  (format stream "}~%~%")
  ;; Write the StarPU function that invokes the CUDA kernel.
  (format stream "extern \"C\" {~%")
  (format stream "void ~A (void *buffers[], void *args) {~%" name)
  (write-prologue name stream)
  ;; Invoke the CUDA kernel.
  (case *iteration-space-rank*
    (0
     (format stream "  dim3 tpb(1);~%")
     (format stream "  dim3 nb(1);~%"))
    (1
     (format stream "  uint64_t size0 = (end0 - start0) / step0;~%")
     (format stream "  dim3 tpb(min((uint64_t)256, size0));~%")
     (format stream "  dim3 nb(1 + ((size0-1) / tpb.x));~%"))
    (2
     (format stream "  uint64_t size0 = (end0 - start0) / step0;~%")
     (format stream "  uint64_t size1 = (end1 - start1) / step1;~%")
     (format stream "  dim3 tpb(min((uint64_t)32, size1), min((uint64_t)16, size0));~%")
     (format stream "  dim3 nb(1 + ((size1-1) / tpb.x), 1 + ((size0-1) / tpb.y));~%"))
    (t
     (format stream "  uint64_t size0 = (end0 - start0) / step0;~%")
     (format stream "  uint64_t size1 = (end1 - start1) / step1;~%")
     (format stream "  uint64_t size2 = (end2 - start2) / step2;~%")
     (format stream "  dim3 tpb(min((uint64_t)32, size2), min((uint64_t)4, size1), min((uint64_t)8, size0));~%")
     (format stream "  dim3 nb(1 + ((size2-1) / tpb.x), 1 + ((size1-1) / tpb.y), 1 + ((size0-1) / tpb.z));~%")))
  (format stream "  ~A_impl<<<nb, tpb, 0, starpu_cuda_get_local_stream()>>>(~{~A~^, ~});~%"
          name
          (append
           (loop for axis below *iteration-space-rank*
                 collect (format nil "start~D" axis)
                 collect (format nil "end~D" axis)
                 collect (format nil "step~D" axis))
           (loop for (ntype irefs) in *dst-array-info* for axis from 0
                 for type = (ntype-c-type ntype)
                 for rank = (length irefs)
                 collect (format nil "dst~D" axis)
                 collect (format nil "dst~Dskip" axis)
                 append (loop for index below (1- rank) collect (format nil "dst~Ds~D" axis index)))
           (loop for (ntype irefs) in *src-array-info* for axis from 0
                 for type = (ntype-c-type ntype)
                 for rank = (length irefs)
                 collect (format nil "src~D" axis)
                 collect (format nil "src~Dskip" axis)
                 append (loop for index below (1- rank) collect (format nil "src~Ds~D" axis index)))
           (kernel-coeffs)))
  (format stream "  cudaError_t status = cudaGetLastError();~%")
  (format stream "  if (status != cudaSuccess) STARPU_CUDA_REPORT_ERROR(status);~%")
  (format stream "  cudaStreamSynchronize(starpu_cuda_get_local_stream());~%")
  (write-epilogue name stream)
  (format stream "}~%")
  (format stream "}~%"))
