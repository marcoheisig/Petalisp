;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir-backend)

;;; The purpose of the IR backend is to check that the IR conversion
;;; preserves semantics.  It is similar to the reference backend, but
;;; evaluates kernels instead of individual strided arrays.

(defclass ir-backend (backend)
  ())

(defun make-ir-backend ()
  (make-instance 'ir-backend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data Flow Graph Evaluation

(defvar *nodes*)

(defvar *worklist*)

(defstruct node
  (kernel nil :type kernel)
  (dependencies '() :type list)
  (users '() :type list))

(defmethod compute-immediates ((lazy-arrays list) (ir-backend ir-backend))
  (let ((root-buffers (ir-from-lazy-arrays lazy-arrays))
        (*nodes* (make-hash-table :test #'eq))
        (*worklist* '()))
    (map-buffers-and-kernels
     ;; Ensure that each buffer has an attached storage array.
     (lambda (buffer)
       (unless (buffer-storage buffer)
         (setf (buffer-storage buffer)
               (make-array
                (shape-dimensions (buffer-shape buffer))
                :element-type (petalisp.type-inference:type-specifier
                               (buffer-ntype buffer))))))
     ;; Ensure that the dependencies of each kernel are set up properly.
     (lambda (kernel)
       (let ((node (ensure-node kernel)))
         (map-kernel-inputs
          (lambda (buffer)
            (map-buffer-inputs
             (lambda (other-kernel)
               (let ((other-node (ensure-node other-kernel)))
                 (pushnew other-node (node-dependencies node))
                 (pushnew node (node-users other-node))))
             buffer))
          kernel)
         (when (null (node-dependencies node))
           (push node *worklist*))))
     root-buffers)
    (loop until (null *worklist*) do
      (execute-node (pop *worklist*)))
    (mapcar #'immediate-from-buffer root-buffers)))

(defun ensure-node (kernel)
  (or (gethash kernel *nodes*)
      (setf (gethash kernel *nodes*)
            (make-node :kernel kernel))))

(defun execute-node (node)
  (let ((kernel (node-kernel node)))
    (interpret-kernel kernel (kernel-iteration-space kernel)))
  (loop for other-node in (node-users node) do
    (with-accessors ((dependencies node-dependencies)) other-node
      (setf dependencies (remove node dependencies))
      (when (null dependencies)
        (push other-node *worklist*)))))

(defun immediate-from-buffer (buffer)
  (make-instance 'array-immediate
    :ntype (buffer-ntype buffer)
    :shape (buffer-shape buffer)
    :storage (buffer-storage buffer)))
