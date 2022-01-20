;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.xmas-backend)

(defstruct (evaluator
            (:predicate evaluatorp)
            (:copier nil)
            (:constructor %make-evaluator))
  (program nil
   :type program
   :read-only t)
  ;; A vector mapping each buffer number to that buffer's storage array.
  (buffer-storage-vector nil
   :type simple-vector
   :read-only t)
  ;; A vector mapping each kernel number to an optimized function for
  ;; executing that kernel.
  (kernel-fn-vector nil
   :type simple-vector
   :read-only t))

(declaim (type evaluator *evaluator*))
(defvar *evaluator*)

(define-symbol-macro *kernel-fn-vector* (evaluator-kernel-fn-vector *evaluator*))
(define-symbol-macro *buffer-storage-vector* (evaluator-buffer-storage-vector *evaluator*))

(macrolet ((define-accessor (name vector type number-reader)
             `(progn
                (declaim (inline ,name))
                (defun ,name (,type)
                  (declare (,type ,type))
                  (svref ,vector (,number-reader ,type)))
                (declaim (inline (setf ,name)))
                (defun (setf ,name) (value ,type)
                  (declare (,type ,type))
                  (setf (svref ,vector (,number-reader ,type))
                        value))))
           (define-kernel-accessor (name vector)
             `(define-accessor ,name ,vector kernel kernel-number))
           (define-buffer-accessor (name vector)
             `(define-accessor ,name ,vector buffer buffer-number))
           (define-task-accessor (name vector)
             `(define-accessor ,name ,vector task task-number)))
  (define-kernel-accessor kernel-fn *kernel-fn-vector*)
  (define-buffer-accessor %buffer-storage *buffer-storage-vector*))

;; Execution strategy
;;
;; 1. (optional) Add additional task dependency nodes.
;;
;; 2. Allocate the storage of each buffer (liveness, coloring, memory pool, ...)
;;
;; 3. Start executing tasks,

(defun make-evaluator (program)
  (with-accessors ((number-of-kernels program-number-of-kernels)
                   (number-of-buffers program-number-of-buffers)
                   (number-of-tasks program-number-of-tasks)
                   (initial-task program-initial-task)
                   (final-task program-final-task)) program
    (let ((*evaluator*
            (%make-evaluator
             :ir-program program
             :buffer-storage-vector
             (make-array number-of-buffers :initial-element '.invalid-buffer-storage.)
             :kernel-fn-vector
             (make-array number-of-kernels :initial-element '.invalid-kernel-fn))))
      ;; (optional) add additional task dependencies.
      'TODO
      ;; Allocate the storage of each buffer.
      (let ((buffers (make-array number-of-buffers))
            (liveness (make-array number-of-tasks)))
        (map-program-buffers
         (lambda (buffer)
           (setf (svref buffers (buffer-number buffer))
                 buffer))
         program)
        (setf buffers (stable-sort buffers #'shape< :key #'buffer-shape))
        (setf buffers (stable-sort buffers #'petalisp.type-inference:ntype< :key #'buffer-ntype))
        )
      )))

(defun run-evaluator (evaluator backend)
  )

(defun execute-buffer ()
  )

(defun execute-kernel (kernel ))
