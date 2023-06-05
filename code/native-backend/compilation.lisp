;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defmethod target-function
    ((backend backend))
  (values
   'svref
   '(values storage &optional)))

(defmethod source-function
    ((backend backend))
  (values
   'svref
   '(values storage &optional)))

(defmethod unpack-function
    ((backend backend)
     (ntype typo:ntype)
     (rank integer))
  (values
   'native-backend-unpack
   `(values
     cffi:foreign-pointer
     index
     ,@(loop for axis below rank
             collect
             (if (= axis (1- rank))
                 '(eql 1)
                 'index))
     &optional)))

(defun native-backend-unpack (storage denv)
  (declare (storage storage) (denv denv))
  (let* ((offset (- (storage-offset storage)))
         (strides (storage-strides storage))
         (allocation (storage-allocation storage))
         (category (allocation-category allocation))
         (color  (allocation-color allocation))
         (pointers (denv-pointers denv))
         (pointer (svref (svref pointers category) color)))
    (macrolet ((stride (index)
                 `(svref strides ,index)))
      (case (length strides)
        (0 (values pointer offset))
        (1 (values pointer offset (stride 0)))
        (2 (values pointer offset (stride 0) (stride 1)))
        (3 (values pointer offset (stride 0) (stride 1) (stride 2)))
        (4 (values pointer offset (stride 0) (stride 1) (stride 2) (stride 3)))
        (5 (values pointer offset (stride 0) (stride 1) (stride 2) (stride 3) (stride 4)))
        (otherwise (apply #'values pointer offset (coerce strides 'list)))))))

(defmethod store-function
    ((backend backend)
     (ntype typo:ntype))
  `(setf ,(load-function backend ntype)))

(defmethod load-function
    ((backend backend)
     (ntype typo:ntype))
  (typo:ntype-subtypecase ntype
    ((unsigned-byte 8) 'u8-memref)
    ((unsigned-byte 16) 'u16-memref)
    ((unsigned-byte 32) 'u32-memref)
    ((unsigned-byte 64) 'u64-memref)
    (single-float 'f32-memref)
    (double-float 'f64-memref)
    (t
     (if (typo:ntype= ntype (typo:universal-ntype))
         'obj-memref
         (error "Don't know how to load objects of type ~S."
                (typo:ntype-type-specifier ntype))))))

(macrolet ((def (name sap-ref element-type scaling)
             `(progn
                (declaim (inline ,name))
                (defun ,name (pointer offset)
                  (declare (cffi:foreign-pointer pointer) (index offset))
                  (the ,element-type
                       (,sap-ref pointer (index* offset ,scaling))))
                (declaim (inline (setf ,name)))
                (defun (setf ,name) (value pointer offset)
                  (declare (type ,element-type value))
                  (declare (cffi:foreign-pointer pointer) (index offset))
                  (setf (,sap-ref pointer (index* offset ,scaling))
                        value)))))
  (def f64-memref sb-sys:sap-ref-double double-float 8)
  (def f32-memref sb-sys:sap-ref-single single-float 4)
  (def u8-memref sb-sys:sap-ref-8 (unsigned-byte 8) 1)
  (def u16-memref sb-sys:sap-ref-16 (unsigned-byte 16) 2)
  (def u32-memref sb-sys:sap-ref-32 (unsigned-byte 32) 4)
  (def u64-memref sb-sys:sap-ref-64 (unsigned-byte 64) 8)
  (def obj-memref sb-sys:sap-ref-lispobj t 8))
