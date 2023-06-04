;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.codegen)

;;; The IR backend converts its supplied arrays to an equivalent IR graph,
;;; binds each buffer to a Lisp array, and directly interprets each kernel
;;; in an order that respects all data dependencies.
;;;
;;; The IR backend is not particularly fast or sophisticated.  Its main
;;; purpose is for testing.  We compare the solutions computed by the IR
;;; backend with those computed by the reference backend to ensure that the
;;; IR conversion preserves semantics.

(defclass ir-backend (backend)
  ((%mode
    :initarg :mode
    :initform :interpreted
    :reader ir-backend-mode
    :type (member :interpreted :compiled))
   (%compile-cache
    :initform (make-hash-table)
    :reader ir-backend-compile-cache
    :type hash-table)))

(defun make-ir-backend (&key (mode :interpreted))
  (make-instance 'ir-backend
    :mode mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data Flow Graph Evaluation

;;; The *NODES* vector maps each kernel of the IR to a specifications of
;;; its users and dependencies.
(defvar *nodes*)

;; The *CHUNKS* vector maps each buffer of the IR to the root of a tree of
;; chunks.
(defvar *chunks*)

;;; The *WORKLIST* contains all nodes whose dependencies have already been
;;; computed, but which haven't been computed themselves.
(defvar *worklist*)

(defstruct node
  (kernel nil :type kernel)
  (dependencies '() :type list)
  (users '() :type list)
  (fn nil :type (or function null)))

(defmethod backend-compute
    ((ir-backend ir-backend)
     (lazy-arrays list))
  (let* ((program (program-from-lazy-arrays lazy-arrays))
         (*nodes* (make-hash-table :test #'eq))
         (*worklist* '()))
    (map-buffers-and-kernels
     ;; Ensure that each buffer has an attached storage array.
     (lambda (buffer)
       (unless (buffer-storage buffer)
         (setf (buffer-storage buffer)
               (make-array
                (shape-dimensions (buffer-shape buffer))
                :element-type (typo:ntype-type-specifier (buffer-ntype buffer))))))
     ;; Ensure that the dependencies of each kernel are set up properly.
     (lambda (kernel)
       (let ((node (ensure-node kernel ir-backend)))
         (do-kernel-inputs (buffer kernel)
           (do-buffer-inputs (other-kernel buffer)
             (let ((other-node (ensure-node other-kernel ir-backend)))
               (pushnew other-node (node-dependencies node))
               (pushnew node (node-users other-node)))))
         (when (null (node-dependencies node))
           (push node *worklist*))))
     (program-root-buffers program))
    (loop until (null *worklist*) do
      (execute-node (pop *worklist*)))
    (loop for buffer in (program-root-buffers program)
          collect
          (make-delayed-array (petalisp.ir:buffer-storage buffer)))))

(defun ensure-node (kernel ir-backend)
  (or (gethash kernel *nodes*)
      (setf (gethash kernel *nodes*)
            (make-node
             :kernel kernel
             :fn (ecase (ir-backend-mode ir-backend)
                   (:compiled
                    (let ((blueprint (kernel-blueprint kernel))
                          (compile-cache (ir-backend-compile-cache ir-backend)))
                      (or (gethash blueprint compile-cache)
                          (setf (gethash blueprint compile-cache)
                                (compile nil (lisp-translate-blueprint ir-backend blueprint))))))
                   (:interpreted
                    #'interpret-kernel))))))

(defun execute-node (node)
  (let ((kernel (node-kernel node)))
    (funcall (node-fn node)
             kernel
             (kernel-iteration-space kernel)
             (kernel-targets kernel)
             (kernel-sources kernel)
             nil))
  (loop for other-node in (node-users node) do
    (with-accessors ((dependencies node-dependencies)) other-node
      (setf dependencies (remove node dependencies))
      (when (null dependencies)
        (pushnew other-node *worklist*)))))

(defmethod target-function ((ir-backend ir-backend))
  'ir-backend-array)

(defmethod source-function ((ir-backend ir-backend))
  'ir-backend-array)

(defun ir-backend-array (alist index)
  (declare (list alist))
  (buffer-storage (car (nth index alist))))

(defmethod unpack-function
    ((ir-backend ir-backend)
     (ntype typo:ntype)
     (rank integer))
  'unpack-array)

(defmethod store-function
    ((ir-backend ir-backend)
     (ntype typo:ntype))
  '(setf row-major-aref))

(defmethod load-function
    ((ir-backend ir-backend)
     (ntype typo:ntype))
  'row-major-aref)
