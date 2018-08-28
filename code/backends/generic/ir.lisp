;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric make-buffer (strided-array backend))

(defgeneric make-kernel (root iteration-space leaf-test backend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass ir-node ()
  ((%shape :initarg :shape :reader shape)
   (%inputs :initarg :inputs :accessor inputs)
   (%outputs :initarg :outputs :accessor outputs)))

(defclass buffer (ir-node)
  ((%element-type :initarg :element-type :reader element-type)))

(defclass array-buffer (buffer)
  (;; A mapping from indices of the array's shape to storage indices.
   (%transformation :initarg :transformation :accessor transformation)
   ;; The storage of the buffer.  Initially, the storage of a buffer is
   ;; NIL.  Before kernels can write to this buffer, its storage slot must
   ;; be set to an array of the appropriate size.
   (%storage :initarg :storage :initform nil :accessor storage)))

(defclass kernel (ir-node)
  ((%functions :initarg :functions :reader functions)
   (%blueprint :initarg :blueprint :reader blueprint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod make-buffer ((strided-array strided-array) (backend backend))
  (make-instance 'buffer
    :shape (shape strided-array)))

(defmethod initialize-instance ((array-buffer array-buffer) &key &allow-other-keys)
  (prog1 (call-next-method)
    (setf (transformation array-buffer)
          (collapsing-transformation (shape array-buffer)))))

(defmethod make-kernel ((root strided-array)
                        (leaf-function function)
                        (iteration-space shape)
                        (backend backend))
  (let ((input-buffers '())
        (functions '()))
    (labels ((walk-reference (immediate transformation)
               (blueprint/reference
                (id immediate input-buffers)
                (compose-transformations (transformation immediate) transformation)))
             (walk (node relevant-space transformation)
               (unless (set-emptyp relevant-space)
                 (if-let ((leaf (funcall leaf-function node)))
                   (walk-reference leaf transformation)
                   (etypecase node
                     (application
                      (dx-flet ((walk-input (input) (walk input relevant-space transformation)))
                        (let ((operator (operator node)))
                          (blueprint/call
                           (if (symbolp operator)
                               operator
                               (id operator functions))
                           (ucons:map-ulist #'walk-input (inputs node))))))
                     (reduction
                      (let* ((input (input node))
                             (reduction-range (last-elt (ranges (shape input))))
                             (scale (range-step reduction-range))
                             (offset (range-start reduction-range)))
                        (setf (aref bounds bounds-index)
                              (set-size reduction-range))
                        (incf bounds-index)
                        (let ((relevant-space (enlarge-shape relevant-space (shape input)))
                              (transformation (enlarge-transformation transformation scale offset)))
                          (blueprint/reduce
                           (operator node)
                           (walk input relevant-space transformation)))))
                     (fusion ;; the relevant space is already chosen to eliminate fusions
                      (let* ((input (find relevant-space (inputs node)
                                          :key #'shape
                                          :test #'set-intersectionp))
                             (relevant-space (set-intersection relevant-space (shape input))))
                        (walk input relevant-space transformation)))
                     (reference ;; eliminate/lift input-buffers
                      (let ((relevant-space
                              (set-intersection
                               (shape (input node))
                               (transform relevant-space (transformation node))))
                            (transformation
                              (compose-transformations (transformation node) transformation)))
                        (walk (input node) relevant-space transformation))))))))
      (let ((blueprint-body
              (blueprint/store
               (walk-reference target normalizing-transformation)
               (walk root relevant-space normalizing-transformation))))
        (make-instance 'kernel
          :bounds bounds
          :input-buffers (subseq references 0 nil)
          :unknown-functions (subseq unknown-functions 0 nil)
          :blueprint
          (blueprint/with-metadata bounds input-buffers blueprint-body))))))
