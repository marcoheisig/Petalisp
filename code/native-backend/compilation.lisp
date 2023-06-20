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
  (declare (optimize (speed 3) (safety 0)))
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
    ((unsigned-byte 1) 'u1-memref)
    ((unsigned-byte 2) 'u2-memref)
    ((unsigned-byte 4) 'u4-memref)
    ((signed-byte 8) 's8-memref)
    ((unsigned-byte 8) 'u8-memref)
    ((signed-byte 16) 's16-memref)
    ((unsigned-byte 16) 'u16-memref)
    ((signed-byte 32) 's32-memref)
    ((unsigned-byte 32) 'u32-memref)
    ((signed-byte 64) 's64-memref)
    ((unsigned-byte 64) 'u64-memref)
    (base-char 'base-char-memref)
    (character 'character-memref)
    (single-float 'f32-memref)
    (double-float 'f64-memref)
    ((complex single-float) 'c64-memref)
    ((complex double-float) 'c128-memref)
    (t
     (if (>= (typo:ntype-bits ntype) 64)
         'obj-memref
         (error "Don't know how to load objects of type ~S."
                (typo:ntype-type-specifier ntype))))))

(macrolet ((defgetter (name (pointer offset) &body body)
             `(progn (declaim (inline ,name))
                     (defun ,name (,pointer ,offset)
                       (declare (cffi:foreign-pointer ,pointer)
                                (index ,offset))
                       ,@body)))
           (defsetter (name (value pointer offset) &body body)
             `(progn (declaim (inline (setf ,name)))
                     (defun (setf ,name) (,value ,pointer ,offset)
                       (declare (cffi:foreign-pointer ,pointer)
                                (index ,offset))
                       ,@body)))
           (defmemref (name type element-type)
             `(progn (defgetter ,name (pointer offset)
                       (the ,element-type
                            (cffi:mem-aref pointer ,type offset)))
                     (defsetter ,name (value pointer offset)
                       (declare (type ,element-type value))
                       (setf (cffi:mem-aref pointer ,type offset)
                             value))))
           (defbitref (name bits)
             `(progn (defgetter ,name (pointer offset)
                       (multiple-value-bind (byte-index thing-index)
                           (floor offset ,(/ 8 bits))
                         (ldb (byte ,bits (* ,bits thing-index))
                              (cffi:mem-aref pointer :uint8 byte-index))))
                     (defsetter ,name (value pointer offset)
                       (declare (type (unsigned-byte ,bits) value))
                       (multiple-value-bind (byte-index thing-index)
                           (floor offset ,(/ 8 bits))
                         (setf (ldb (byte ,bits (* ,bits thing-index))
                                    (cffi:mem-aref pointer :uint8 byte-index))
                               value))))))
  (defbitref u1-memref 1)
  (defbitref u2-memref 2)
  (defbitref u4-memref 4)
  (defmemref u8-memref :uint8 (unsigned-byte 8))
  (defmemref u16-memref :uint16 (unsigned-byte 16))
  (defmemref u32-memref :uint32 (unsigned-byte 32))
  (defmemref u64-memref :uint64 (unsigned-byte 64))
  (defmemref s8-memref :int8 (signed-byte 8))
  (defmemref s16-memref :int16 (signed-byte 16))
  (defmemref s32-memref :int32 (signed-byte 32))
  (defmemref s64-memref :int64 (signed-byte 64))
  (defmemref f32-memref :float single-float)
  (defmemref f64-memref :double double-float)
  ;; Complex numbers.
  (defgetter c64-memref (pointer offset)
    (let ((index (* offset 8)))
      (complex (cffi:mem-ref pointer :float index)
               (cffi:mem-ref pointer :float (+ index 4)))))
  (defsetter c64-memref (value pointer offset)
    (declare (type (complex single-float) value))
    (let ((index (* offset 8)))
      (setf (cffi:mem-ref pointer :float index)
            (realpart value))
      (setf (cffi:mem-ref pointer :float (+ index 4))
            (imagpart value))
      value))
  (defgetter c128-memref (pointer offset)
    (let ((index (* offset 16)))
      (complex (cffi:mem-ref pointer :double index)
               (cffi:mem-ref pointer :double (+ index 8)))))
  (defsetter c128-memref (value pointer offset)
    (declare (type (complex double-float) value))
    (let ((index (* offset 16)))
      (setf (cffi:mem-ref pointer :double index)
            (realpart value))
      (setf (cffi:mem-ref pointer :double (+ index 8))
            (imagpart value))
      value))
  ;; Base Chars
  (defgetter base-char-memref (pointer offset)
    (code-char
     (cffi:mem-aref pointer :uint8 offset)))
  (defsetter base-char-memref (value pointer offset)
    (declare (character value))
    (setf (cffi:mem-aref pointer :uint8 offset)
          (char-code value)))
  ;; Characters
  (defgetter character-memref (pointer offset)
    (code-char
     (cffi:mem-aref pointer :uint32 offset)))
  (defsetter character-memref (value pointer offset)
    (declare (character value))
    (setf (cffi:mem-aref pointer :uint32 offset)
          (char-code value)))
  ;; Boxed Objects
  (defgetter obj-memref (pointer offset)
    #+sbcl (sb-kernel:%make-lisp-obj
            (sb-sys:sap-ref-word pointer (index* 8 offset)))
    #-(or sbcl)
    (error "Not implemented yet."))
  (defsetter obj-memref (value pointer offset)
    #+ccl (ccl::%setf-macptr-to-object (cffi:mem-aptr :pointer offset) value)
    #+sbcl (setf (sb-sys:sap-ref-word pointer (index* 8 offset))
                 (sb-kernel:get-lisp-obj-address value))
    #-(or ccl sbcl)
    (error "Not implemented yet.")))
