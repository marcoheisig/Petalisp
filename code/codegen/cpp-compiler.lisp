;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.codegen)

(defun bpvalue-cpp-string (bpvalue)
  (etypecase bpvalue
    (bpvariable
     (string-downcase
      (symbol-name
       (bpvariable-name bpvalue))))
    (bpconstant
     (let ((value (bpconstant-value bpvalue)))
       (etypecase value
         (single-float (format nil "~,,,,,,'EEF" value))
         (double-float (format nil "~,,,,,,'EE" value))
         (integer (format nil "~D" value)))))))

(defun bpvalue-cpp-type (bpvalue)
  (let ((ntype (bpvalue-ntype bpvalue)))
    (if (typo:ntype= ntype (typo:universal-ntype))
        "void*"
        (ntype-cpp-info ntype))))

(defun bpvalue-cffi-type (bpvalue)
  (let ((ntype (bpvalue-ntype bpvalue)))
    (typo:ntype-subtypecase ntype
      (short-float :float)
      (single-float :float)
      (double-float :double)
      (long-float :double)
      ((signed-byte 8) :int8)
      ((unsigned-byte 8) :uint8)
      ((signed-byte 16) :int16)
      ((unsigned-byte 16) :uint16)
      ((signed-byte 32) :int32)
      ((unsigned-byte 32) :uint32)
      ((signed-byte 64) :int64)
      ((unsigned-byte 64) :uint64)
      (t :pointer))))

(defun cpp-translate-blueprint (client blueprint)
  (let* ((bpinfo (blueprint-bpinfo blueprint))
         (arguments (bpinfo-cpp-arguments bpinfo))
         (name (format nil "PETALISP_CPP_KERNEL_~X" (random most-positive-fixnum))))
    (make-kernel-lambda
     client
     bpinfo
     `(let ((.foreign-library.
              (load-time-value
               (load-foreign-code
                ,(with-output-to-string (stream)
                   (cpp-write-defun bpinfo name arguments stream))
                :flags '("-O3" "-march=native" "-fPIC")))))
        (declare (ignore .foreign-library.))
        (cffi:foreign-funcall
         ,name
         ,@(loop for argument in arguments
                 collect (bpvalue-cffi-type argument)
                 collect (bpvariable-name argument)))))))

(defun cpp-compile-blueprint (client blueprint)
  (compile nil (cpp-translate-blueprint client blueprint)))

(defun bpinfo-cpp-arguments (bpinfo)
  (let ((rargs '()))
    (labels ((collect (bpvariable)
               (declare (bpvariable bpvariable))
               (push bpvariable rargs))
             (maybe-collect (bpvalue)
               (declare (bpvalue bpvalue))
               (when (bpvariablep bpvalue)
                 (collect bpvalue))))
      (loop for bprange across (bpinfo-ranges bpinfo) do
        (maybe-collect (bprange-start bprange))
        (maybe-collect (bprange-end bprange))
        (maybe-collect (bprange-step bprange)))
      (let ((bpmemrefs
              (concatenate
               'vector
               (bpinfo-targets bpinfo)
               (bpinfo-sources bpinfo))))
        (loop for bpmemref across bpmemrefs do
          (maybe-collect (bpmemref-base bpmemref))
          (maybe-collect (bpmemref-start bpmemref))
          (map nil #'maybe-collect (bpmemref-strides bpmemref))
          (loop for bpstencil across (bpmemref-stencils bpmemref) do
            (loop for offset across (bpstencil-offsets bpstencil)
                  for scaling across (bpstencil-scalings bpstencil)
                  do (maybe-collect offset)
                  do (maybe-collect scaling)))))
      (loop for bpinstruction across (bpinfo-instructions bpinfo) do
        (when (typep bpinstruction 'bpiref)
          (maybe-collect (bpiref-offset bpinstruction))
          (maybe-collect (bpiref-scaling bpinstruction)))))
    (reverse rargs)))

(defun cpp-write-defun (bpinfo name arguments stream)
  (terpri stream)
  (format stream "#include <tgmath.h>~%")
  (format stream "extern \"C\" {~%")
  (format stream "void ~A(~{~A ~A~^, ~}) {~%"
          name
          (mapcan #'cpp-translate-argument arguments))
  (cpp-write-instructions bpinfo 0 stream)
  (cpp-write-loop bpinfo 1 stream)
  (format stream "} // function~%")
  (format stream "} // extern \"C\" ~%"))

(defun cpp-translate-argument (argument)
  (assert (bpvariablep argument))
  (list
   (bpvalue-cpp-type argument)
   (bpvalue-cpp-string argument)))

(defun cpp-write-loop (bpinfo level stream)
  (unless (= level (length (bpinfo-levels bpinfo)))
    (let ((indent (make-sequence 'string (* 2 level) :initial-element #\space)))
      (let* ((axis (1- level))
             (bpindex (aref (bpinfo-indices bpinfo) axis))
             (bprange (aref (bpinfo-ranges bpinfo) axis))
             (bpstart (bprange-start bprange))
             (bpstep (bprange-step bprange))
             (bpend (bprange-end bprange)))
        (format stream "~Afor(int64_t ~A = ~A; ~A < ~A; ~A += ~A) {~%"
                indent
                (bpvalue-cpp-string bpindex)
                (bpvalue-cpp-string bpstart)
                (bpvalue-cpp-string bpindex)
                (bpvalue-cpp-string bpend)
                (bpvalue-cpp-string bpindex)
                (bpvalue-cpp-string bpstep))
        (cpp-write-instructions bpinfo level stream)
        (cpp-write-loop bpinfo (1+ level) stream)
        (format stream "~A}~%" indent)))))

(defun cpp-write-instructions (bpinfo level stream)
  (let ((indent (make-sequence 'string (* 2 (1+ level)) :initial-element #\space)))
    (loop for bpinstruction in (aref (bpinfo-levels bpinfo) level) do
      (etypecase bpinstruction
        (bpcall
         (multiple-value-bind (type fn kind)
             (function-cpp-info
              (bpconstant-value
               (bpcall-function bpinstruction)))
           (ecase kind
             (:infix
              (format stream "~A~A ~A = ~A ~A ~A;~%"
                      indent
                      type
                      (bpvalue-cpp-string (bpcall-value bpinstruction))
                      (bpvalue-cpp-string (bpcall-argument bpinstruction 0))
                      fn
                      (bpvalue-cpp-string (bpcall-argument bpinstruction 1))))
             (:prefix
              (format stream "~A~A ~A = ~A(~{~A~});~%"
                      indent
                      type
                      (bpvalue-cpp-string (bpcall-value bpinstruction))
                      fn
                      (map 'list #'bpvalue-cpp-string (bpcall-arguments bpinstruction)))))))
        (bpload
         (let ((bpmemref (bpload-source bpinstruction)))
           (format stream "~A~A ~A = ((~A*)~A)[~A];~%"
                   indent
                   (ntype-cpp-info (bpmemref-ntype bpmemref))
                   (bpvalue-cpp-string (bpinstruction-value bpinstruction))
                   (ntype-cpp-info (bpmemref-ntype bpmemref))
                   (bpvalue-cpp-string (bpmemref-base bpmemref))
                   (bpvalue-cpp-string (bpload-index bpinstruction)))))
        (bpstore
         (let ((bpmemref (bpstore-target bpinstruction)))
           (format stream "~A((~A*)~A)[~A] = ~A;~%"
                   indent
                   (ntype-cpp-info (bpmemref-ntype bpmemref))
                   (bpvalue-cpp-string (bpmemref-base bpmemref))
                   (bpvalue-cpp-string (bpstore-index bpinstruction))
                   (bpvalue-cpp-string (bpstore-value bpinstruction)))))
        (bpiref (values))))))
