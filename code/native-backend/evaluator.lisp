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
  (program nil :type program :read-only t)
  (schedule nil :type list :read-only t)
  (result-shapes nil :type (simple-array shape (*)) :read-only t)
  (result-ntypes nil :type (simple-array typo:ntype (*)) :read-only t)
  (result-allocations nil :type (simple-array allocation (*)) :read-only t)
  (argument-shapes nil :type (simple-array shape (*)) :read-only t)
  (argument-ntypes nil :type (simple-array typo:ntype (*)) :read-only t)
  (argument-allocations nil :type (simple-array allocation (*)) :read-only t)
  (constant-arrays nil :type (simple-array array (*)) :read-only t)
  (constant-allocations nil :type (simple-array allocation (*)) :read-only t)
  (worker-temporary-allocations nil :type (simple-array (simple-array allocation (*)) (*)) :read-only t))

(defun make-cenv (backend unknowns lazy-arrays)
  (let* ((program (program-from-lazy-arrays lazy-arrays))
         ;; Allocate result vectors.
         (nresults (length lazy-arrays))
         (result-shapes (make-array nresults))
         (result-ntypes (make-array nresults))
         (result-allocations (make-array nresults))
         ;; Allocate argument vectors.
         (narguments (length unknowns))
         (argument-shapes (make-array narguments))
         (argument-ntypes (make-array narguments))
         (argument-allocations (make-array narguments))
         ;; Allocate constant vectors.
         (nconstants (count-if-not #'lazy-unknown-p (program-leaf-alist program) :key #'cdr))
         (constant-arrays (make-array nconstants))
         (constant-allocations (make-array nconstants))
         ;; Allocate per-worker temporary vectors.
         (nworkers (worker-pool-size (backend-worker-pool backend)))
         (worker-temporary-allocations (make-array nworkers))
         ;; Partition the program and compute a schedule.
         (primogenitor-buffer-shard-vector (partition-program program))
         (schedule (compute-program-schedule program primogenitor-buffer-shard-vector)))
    ;; Bind the result buffers and allocations.
    (loop for buffer in (program-root-buffers program) for index from 0 do
      (let* ((number (buffer-number buffer))
             (buffer-shard (aref primogenitor-buffer-shard-vector number))
             (layout (buffer-shard-layout buffer-shard))
             (storage (layout-storage layout)))
        (setf (aref result-shapes index)
              (buffer-shard-shape buffer-shard))
        (setf (aref result-ntypes index)
              (storage-ntype storage))
        (setf (aref result-allocations index)
              (storage-allocation storage))))
    ;; Bind the argument buffers and allocations.
    (let ((constant-number 0))
      (loop for (buffer . lazy-array) in (program-leaf-alist program) do
        (let* ((number (buffer-number buffer))
               (buffer-shard (aref primogenitor-buffer-shard-vector number))
               (layout (buffer-shard-layout buffer-shard))
               (storage (layout-storage layout))
               (delayed-action (lazy-array-delayed-action lazy-array)))
          (etypecase delayed-action
            (delayed-array
             (setf (aref constant-arrays constant-number)
                   (delayed-array-storage delayed-action))
             (setf (aref constant-allocations constant-number)
                   (storage-allocation storage))
             (incf constant-number))
            (delayed-unknown
             (let ((argument-number (position lazy-array unknowns)))
               (assert (< -1 argument-number narguments))
               (setf (aref argument-shapes argument-number)
                     (buffer-shard-shape buffer-shard))
               (setf (aref argument-ntypes argument-number)
                     (storage-ntype storage))
               (setf (aref argument-allocations argument-number)
                     (storage-allocation storage))))))))
    (%make-cenv
     :backend backend
     :program program
     :schedule schedule
     :result-shapes result-shapes
     :result-ntypes result-ntypes
     :result-allocations result-allocations
     :argument-shapes argument-shapes
     :argument-ntypes argument-ntypes
     :argument-allocations argument-allocations
     :constant-arrays constant-arrays
     :constant-allocations constant-allocations
     :worker-temporary-allocations worker-temporary-allocations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Dynamic Environment

(defstruct (denv
            (:predicate denvp)
            (:constructor %make-denv))
  (cenv (alexandria:required-argument :denv)
   :type cenv
   :read-only t)
  (worker-pointer-vector (alexandria:required-argument :worker-pointer-vector)
   :type (simple-array (simple-array cffi:foreign-pointer (*)) (*))
   :read-only t)
  (result-arrays (alexandria:required-argument :result-arrays)
   :type (simple-array array (*))
   :read-only t))

(defun bind-result (denv result index)
  (with-slots (cenv worker-pointer-vector) denv
    (with-slots (result-allocations result-buffers result-arrays) cenv
      (let ((allocation (aref result-allocations index))
            (buffer (aref result-buffers index)))
        (if (not result)
            (setf result (make-buffer-like-array buffer))
            (ensure-array-buffer-compatibility result buffer))
        (setf (aref result-arrays index) result)
        (with-slots (worker-id color) allocation
          (let ((pointer-vector (aref worker-pointer-vector worker-id)))
            (assert (cffi:null-pointer-p (aref pointer-vector color)))
            (setf (aref pointer-vector color)
                  (array-storage-pointer result))))))))

(defun get-result (denv index)
  (declare (denv denv))
  (aref (denv-result-arrays denv) index))

(defun bind-argument (denv argument index)
  (with-slots (cenv worker-pointer-vector) denv
    (with-slots (argument-allocations argument-buffers) cenv
      (let ((allocation (aref argument-allocations index))
            (buffer (aref argument-buffers index)))
        (ensure-array-buffer-compatibility argument buffer)
        (with-slots (worker-id color) allocation
          (let ((pointer-vector (aref worker-pointer-vector worker-id)))
            (assert (cffi:null-pointer-p (aref pointer-vector color)))
            (setf (aref pointer-vector color)
                  (array-storage-pointer argument))))))))

(defun array-storage-pointer (array)
  (sb-kernel:with-array-data ((data array) (start) (end))
    (declare (ignore end))
    (assert (zerop start))
    (sb-sys:vector-sap
     (sb-ext:array-storage-vector data))))

(defun worker-allocate (denv)
  (let* ((worker-id (worker-id *worker*))
         (cenv (denv-cenv denv))
         (temporary-allocations (aref (cenv-worker-temporary-allocations cenv) worker-id))
         (pointer-vector (aref (denv-worker-pointer-vector denv) worker-id)))
    (loop for index below (length temporary-allocations) do
      (let* ((allocation (aref temporary-allocations index))
             (count (expt 2 (allocation-exponent allocation))))
        (setf (aref pointer-vector index)
              (cffi:foreign-alloc :uint8 :count count))))))

(defun worker-execute (denv action)
  (barrier)
  ;; Check error flag.
  (break "TODO")
  ;; Copy all ghost layers.
  (break "TODO")
  (barrier)
  ;; Execute all subtasks.
  (break "TODO"))

(defun worker-free (denv)
  (let* ((worker-id (worker-id *worker*))
         (pointer-vector (aref (denv-worker-pointer-vector denv) worker-id)))
    (loop for index below (length pointer-vector) do
      (cffi:foreign-free
       (shiftf (aref pointer-vector index)
               (cffi:null-pointer))))))

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
    (with-slots (constant-allocations constant-arrays) cenv
      (loop for array across constant-arrays
            for allocation across constant-allocations
            do (with-slots (worker-id color) allocation
                 (let ((pointer-vector (aref worker-pointer-vector worker-id)))
                   (assert (cffi:null-pointer-p (aref pointer-vector color)))
                   (setf (aref pointer-vector color)
                         (array-storage-pointer array))))))
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
