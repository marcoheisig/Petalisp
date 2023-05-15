;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

;;; Each evaluation on a native backend happens in a particular environment.
;;; This environment is composed of a constant part that is shared between all
;;; invocations of the same evaluator, and a dynamic part that is unique for
;;; each invocation of a particular evaluator.  For brevity, we name the former
;;; part the cenv, and the latter the denv.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Constant Environment

(defstruct (cenv
            (:predicate cenvp)
            (:constructor %make-cenv))
  (backend nil :type backend :read-only t)
  (schedule nil :type list :read-only t)
  (result-shapes nil :type (simple-array shape (*)) :read-only t)
  (result-ntypes nil :type (simple-array typo:ntype (*)) :read-only t)
  (argument-shapes nil :type (simple-array shape (*)) :read-only t)
  (argument-ntypes nil :type (simple-array typo:ntype (*)) :read-only t)
  (constant-arrays nil :type (simple-array array (*)) :read-only t)
  ;; A vector with three more entries than there are workers in the backend.
  ;; The first three entries are a vector of constant allocations, a vector of
  ;; result allocations, and a vector of argument allocations.  The remaining
  ;; entries are one vector of local allocations per worker.
  (allocations nil :type (simple-array simple-vector (*)) :read-only t))

(defun make-cenv (backend unknowns lazy-arrays)
  (let* ((program (program-from-lazy-arrays lazy-arrays))
         (primogenitor-buffer-shard-vector (partition-program program))
         (schedule (compute-schedule primogenitor-buffer-shard-vector backend)))
    (multiple-value-bind (allocations constant-arrays)
        (compute-allocations schedule primogenitor-buffer-shard-vector unknowns backend)
      (%make-cenv
       :backend backend
       :schedule schedule
       :result-shapes (map 'vector #'buffer-shape (program-root-buffers program))
       :result-ntypes (map 'vector #'buffer-ntype (program-root-buffers program))
       :argument-shapes (map 'vector #'lazy-array-shape unknowns)
       :argument-ntypes (map 'vector #'lazy-array-ntype unknowns)
       :constant-arrays constant-arrays
       :allocations allocations))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Dynamic Environment

(defstruct (denv
            (:predicate denvp)
            (:constructor %make-denv))
  (cenv (alexandria:required-argument :cenv)
   :type cenv
   :read-only t)
  (result-arrays (alexandria:required-argument :result-arrays)
   :type (simple-array array (*))
   :read-only t)
  ;; A vector of vectors with the same structure as the cenv's allocations,
  ;; which contains the pointer to memory corresponding to each allocation.
  (pointers nil
   :type (simple-array simple-vector (*))
   :read-only t)
  ;; The serious condition that was signaled during evaluation, or NIL if
  ;; everything went smoothly so far.  Once this slot is set to a non-NIL
  ;; value, workers will just skip the remaining evaluation.
  (serious-condition nil
   :type (or null condition)))

(defun make-denv (cenv)
  (%make-denv
   :cenv cenv
   :result-arrays (make-array (length (cenv-result-shapes cenv)) :initial-element nil)
   :pointers
   (map 'vector
        (lambda (vector)
          (make-array (length vector) :initial-element (cffi:null-pointer)))
        (cenv-allocations cenv))))

(defun bind-result (denv result index)
  (with-slots (cenv result-arrays pointers) denv
    (with-slots (result-shapes result-ntypes allocations) cenv
      (let ((shape (aref result-shapes index))
            (ntype (aref result-ntypes index)))
        (if (not result)
            (setf result (make-array-from-shape-and-ntype shape ntype))
            (ensure-array-shape-ntype-compatibility result shape ntype))
        (setf (aref result-arrays index) result)
        (setf (aref (aref pointers +result-allocation-category+) index)
              (array-storage-pointer result))))))

(defun get-result (denv index)
  (declare (denv denv))
  (aref (denv-result-arrays denv) index))

(defun bind-argument (denv argument index)
  (with-slots (cenv pointers) denv
    (with-slots (argument-shapes argument-ntypes) cenv
      (let ((shape (aref argument-shapes index))
            (ntype (aref argument-ntypes index)))
        (ensure-array-shape-ntype-compatibility argument shape ntype)
        (setf (aref (aref pointers +argument-allocation-category+) index)
              (array-storage-pointer argument))))))

(defun array-storage-pointer (array)
  (sb-kernel:with-array-data ((data array) (start) (end))
    (declare (ignore end))
    (assert (zerop start))
    (sb-sys:vector-sap
     (sb-ext:array-storage-vector data))))

(defun worker-allocate (denv)
  (declare (denv denv))
  (with-slots (cenv pointers) denv
    (with-slots (allocations) cenv
      (let* ((worker-id (worker-id *worker*))
             (category (+ worker-id +worker-allocation-category-offset+))
             (local-allocations (aref allocations category))
             (local-pointers (aref pointers category)))
        (loop for index below (length local-allocations) do
          (setf (aref local-pointers index)
                (cffi:foreign-alloc
                 :uint8
                 :count (allocation-size-in-bytes
                         (aref local-allocations index)))))))))

(defun worker-free (denv)
  (declare (denv denv))
  (let* ((worker-id (worker-id *worker*))
         (category (+ worker-id +worker-allocation-category-offset+)))
    (map nil #'cffi:foreign-free (aref (denv-pointers denv) category))))

(defun worker-execute (denv action)
  (declare (denv denv) (action action))
  (worker-synchronize-and-invoke denv (action-copy-invocations action))
  (worker-synchronize-and-invoke denv (action-work-invocations action)))

(defun worker-synchronize-and-invoke (denv invocations)
  (declare (denv denv) (list invocations))
  (barrier)
  (with-slots (pointers serious-condition) denv
    (unless serious-condition
      (handler-case
          (loop for invocation of-type invocation in invocations do
            (funcall (invocation-kfn invocation)
                     (invocation-sources invocation)
                     (invocation-targets invocation)
                     denv))
        (serious-condition (c)
          (atomics:cas serious-condition nil c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Evaluator

(defmethod backend-evaluator
    ((backend backend)
     (unknowns list)
     (lazy-arrays list))
  (funcall (evaluator-template (length unknowns) (length lazy-arrays))
           (make-cenv backend unknowns lazy-arrays)))

(let ((cache (make-hash-table)))
  (defun evaluator-template (number-of-results number-of-arguments)
    (alexandria:ensure-gethash
     number-of-arguments
     (alexandria:ensure-gethash number-of-results cache (make-hash-table)))
    (let ((results (result-variables number-of-results))
          (arguments (result-variables number-of-results)))
      (compile
       nil
       `(lambda (cenv)
          (declare (cenv cenv))
          (let (,@results ,@arguments)
            (let ((denv (make-denv cenv)))
              ,@(loop for result in results for index from 0
                      collect `(bind-result denv ,result ,index))
              ,@(loop for argument in arguments for index from 0
                      collect `(bind-argument denv ,argument ,index))
              (evaluate denv)
              (values
               ,@(loop for index below number-of-results
                       collect `(get-result denv))))))))))

(defun result-variables (n)
  (loop for i below n
        collect (intern (format nil "~A~D" "DST" i) #.*package*)))

(defun argument-variables (n)
  (loop for i below n
        collect (intern (format nil "~A~D" "SRC" i) #.*package*)))

(defun evaluate (denv)
  (with-slots (cenv worker-pointer-vector result-arrays) denv
    ;; Bind all constants.
    (with-slots (allocations constant-arrays) cenv
      (loop for array across constant-arrays
            for allocation across (aref allocations +constant-allocation-category+)
            do (setf (aref (aref worker-pointer-vector +constant-allocation-category+)
                           (allocation-color allocation))
                     (array-storage-pointer array))))
    (with-slots (backend schedule) cenv
      (let* ((worker-pool (backend-worker-pool backend))
             (nworkers (worker-pool-size worker-pool))
             (request (make-request nworkers)))
        ;; Allocate storage on each worker.
        (loop for worker-id below nworkers do
          (worker-enqueue
           (worker-pool-worker worker-pool worker-id)
           (lambda ()
             (worker-allocate denv))))
        ;; Execute the schedule.
        (loop for actions in schedule do
          (loop for worker-id below nworkers do
            (worker-enqueue
             (worker-pool-worker worker-pool worker-id)
             (let ((action (aref actions worker-id)))
               (lambda ()
                 (worker-execute denv action))))))
        ;; Free all storage.
        (loop for worker-id below nworkers do
          (worker-enqueue
           (worker-pool-worker worker-pool worker-id)
           (lambda ()
             (worker-free denv))))
        ;; Signal completion.
        (loop for worker-id below nworkers do
          (worker-enqueue
           (worker-pool-worker worker-pool worker-id)
           (lambda ()
             (with-slots (cell lock cvar) request
               (when (zerop (atomics:atomic-decf (car cell)))
                 (bordeaux-threads:with-lock-held (lock)
                   #+(or) ;; TODO wait for new version of bordeaux threads.
                   (bordeaux-threads:condition-broadcast cvar)
                   (bordeaux-threads:condition-notify cvar)))))))
        ;; Free storage on each worker.
        (loop for worker-id below nworkers do
          (worker-enqueue
           (worker-pool-worker worker-pool worker-id)
           (lambda ()
             (worker-free denv))))))))
