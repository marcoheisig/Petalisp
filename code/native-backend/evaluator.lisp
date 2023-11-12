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
  (constant-storage-vectors nil :type (simple-array vector (*)) :read-only t)
  ;; A vector with three more entries than there are workers in the backend.
  ;; The first three entries are a vector of constant allocations, a vector of
  ;; result allocations, and a vector of argument allocations.  The remaining
  ;; entries are one vector of local allocations per worker.
  (allocations nil :type (simple-array simple-vector (*)) :read-only t))

(defun make-cenv (backend unknowns lazy-arrays)
  (let* ((debug (backend-debug-flag backend))
         (program (program-from-lazy-arrays lazy-arrays :debug debug))
         (primogenitor-buffer-shard-vector (partition-program program :debug debug))
         (schedule (compute-schedule primogenitor-buffer-shard-vector backend)))
    (multiple-value-bind (allocations constant-arrays)
        (compute-allocations schedule primogenitor-buffer-shard-vector unknowns backend)
      (let ((cenv
              (%make-cenv
               :backend backend
               :schedule schedule
               :result-shapes (map 'vector #'buffer-shape (program-root-buffers program))
               :result-ntypes (map 'vector #'buffer-ntype (program-root-buffers program))
               :argument-shapes (map 'vector #'lazy-array-shape unknowns)
               :argument-ntypes (map 'vector #'lazy-array-ntype unknowns)
               :constant-arrays constant-arrays
               :constant-storage-vectors (map 'vector #'array-storage-vector constant-arrays)
               :allocations allocations)))
        (when (backend-debug-flag backend)
          (check-cenv cenv))
        cenv))))

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
  ;; which contains the memory corresponding to each allocation.
  (pointers nil
   :type (simple-array simple-vector (*))
   :read-only t)
  ;; Any serious condition that was signaled during evaluation, or NIL if
  ;; everything went smoothly so far.  Once this slot is set to a non-NIL
  ;; value, workers will just skip the remaining evaluation.
  (serious-conditions nil
   :type list)
  (serious-condition-lock (bordeaux-threads-2:make-lock :name "Denv Serious Condition Lock")
   :type bordeaux-threads-2:lock)
  (request nil :type request))

(defun make-denv (cenv)
  (declare (cenv cenv))
  (%make-denv
   :cenv cenv
   :result-arrays (make-array (length (cenv-result-shapes cenv)) :initial-element nil)
   :request (make-request)
   :pointers
   (map 'vector
        (lambda (vector)
          (make-array (length vector) :initial-element nil))
        (cenv-allocations cenv))))

(defun ensure-result (cenv index result)
  (with-slots (result-shapes result-ntypes allocations) cenv
    (let ((shape (aref result-shapes index))
          (ntype (aref result-ntypes index)))
      (if (not result)
          (make-array-from-shape-and-ntype shape ntype)
          (ensure-array-shape-ntype-compatibility result shape ntype)))))

(defun bind-result (denv result index)
  (with-slots (result-arrays pointers) denv
    (setf (aref result-arrays index) result)
    (setf (aref (aref pointers +result-allocation-category+) index)
          (array-storage-pointer result))))

(defun get-result (denv index)
  (declare (denv denv))
  (let ((result (aref (denv-result-arrays denv) index)))
    (unless (eq (array-element-type result) 't)
      (assert (cffi:pointer-eq
               (aref (aref (denv-pointers denv) +result-allocation-category+) index)
               (array-storage-pointer result))))
    result))

(defun bind-argument (denv argument index)
  (with-slots (cenv pointers) denv
    (with-slots (argument-shapes argument-ntypes) cenv
      (let ((shape (aref argument-shapes index))
            (ntype (aref argument-ntypes index)))
        (ensure-array-shape-ntype-compatibility argument shape ntype)
        (setf (aref (aref pointers +argument-allocation-category+) index)
              (array-storage-pointer argument))))))

(defun array-storage-vector (array)
  #+sbcl (sb-ext:array-storage-vector array)
  #+ccl (multiple-value-bind (vector offset)
                    (ccl::array-data-and-offset array)
                  (assert (zerop offset))
          vector)
  #-(or sbcl ccl) (error "Not implemented yet."))

(defun array-storage-pointer (array)
  (let ((vector (array-storage-vector array)))
    (if (simple-vector-p vector)
        vector
        (static-vectors:static-vector-pointer vector))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions

(define-condition evaluation-failure (error)
  ((%serious-conditions
    :initarg :serious-conditions
    :initform (alexandria:required-argument :serious-conditions)
    :reader serious-conditions))
  (:report
   (lambda (condition stream)
     (format stream "~&Evaluation failed with the following serious conditions:~{~%~% ~A~}"
             (serious-conditions condition)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Evaluator

(defmethod backend-evaluator
    ((backend backend)
     (unknowns list)
     (lazy-arrays list))
  (funcall (evaluator-template (length lazy-arrays) (length unknowns))
           (make-cenv backend unknowns lazy-arrays)))

(let ((cache (make-hash-table)))
  (defun evaluator-template (number-of-results number-of-arguments)
    (alexandria:ensure-gethash
     number-of-arguments
     (alexandria:ensure-gethash number-of-results cache (make-hash-table)))
    (let ((results (result-variables number-of-results))
          (arguments (argument-variables number-of-arguments))
          ;; [ar]sv is short for [argument|result] storage vector.
          (rsvs (loop repeat number-of-results collect (gensym "RSV")))
          (asvs (loop repeat number-of-arguments collect (gensym "ASV"))))
      (compile
       nil
       `(lambda (cenv)
          (declare (cenv cenv) (optimize (safety 3)))
          (lambda (,@results ,@arguments)
            ;; Shadow each result whose value is NIL with a suitable array.
            (let ,(loop for result in results for index from 0
                        collect `(,result (ensure-result cenv ,index ,result)))
              (let ,(loop for array in (append results arguments)
                          for vector in (append rsvs asvs)
                          collect `(,vector (array-storage-vector ,array)))
                (petalisp.utilities:with-pinned-objects (,@rsvs ,@asvs)
                  (let ((denv (make-denv cenv)))
                    (declare (denv denv))
                    ,@(loop for result in results for index from 0
                            collect `(bind-result denv ,result ,index))
                    ,@(loop for argument in arguments for index from 0
                            collect `(bind-argument denv ,argument ,index))
                    (evaluate denv)
                    (values
                     ,@(loop for index below number-of-results
                             collect `(get-result denv ,index)))))))))))))

(defun result-variables (n)
  (loop for i below n
        collect (intern (format nil "~A~D" "DST" i) #.*package*)))

(defun argument-variables (n)
  (loop for i below n
        collect (intern (format nil "~A~D" "SRC" i) #.*package*)))

(defun evaluate (denv)
  (declare (denv denv))
  (with-slots (cenv pointers request serious-conditions) denv
    (with-slots (allocations constant-arrays constant-storage-vectors) cenv
      ;; Pin and bind all constants.
      (petalisp.utilities:with-pinned-objects* constant-storage-vectors
        (loop for array across constant-arrays
              for allocation across (aref allocations +constant-allocation-category+)
              do (setf (aref (aref pointers +constant-allocation-category+)
                             (allocation-color allocation))
                       (array-storage-pointer array))))
      (with-slots (backend schedule) cenv
        (let* ((worker-pool (backend-worker-pool backend))
               (nworkers (worker-pool-size worker-pool)))
          ;; Distribute the work.
          (loop for worker-id below nworkers do
            (worker-enqueue
             (worker-pool-worker worker-pool worker-id)
             (lambda () (worker-evaluate denv))))
          ;; Wait for completion.
          (request-wait request)
          ;; Check whether there were any serious conditions.
          (unless (null serious-conditions)
            (error 'evaluation-failure :serious-conditions serious-conditions)))))))

(defun worker-evaluate (denv)
  (let* ((worker-id (worker-id *worker*))
         (category (+ worker-id +worker-allocation-category-offset+)))
    (with-slots (cenv pointers request) denv
      (with-slots (allocations schedule) cenv
        ;; Allocate memory.
        (let ((local-allocations (aref allocations category))
              (local-pointers (aref pointers category)))
          #+(or) ;; TODO
          (message "Allocating ~,2E bytes of memory."
                   (loop for allocation across local-allocations
                         sum (allocation-size-in-bytes allocation)))
          (loop for index below (length local-allocations) do
            (setf (aref local-pointers index)
                  (allocate-memory (aref local-allocations index)))))
        ;; Execute the schedule.
        (loop for action-vector of-type simple-vector in schedule do
          (let ((action (aref action-vector worker-id)))
            (if (not action)
                (progn
                  (worker-synchronize-and-invoke denv '())
                  (worker-synchronize-and-invoke denv '()))
                (progn
                  (worker-synchronize-and-invoke denv (action-copy-invocations action))
                  (worker-synchronize-and-invoke denv (action-work-invocations action))))))
        ;; Signal completion.
        (barrier)
        (when (zerop worker-id)
          (with-slots (lock cvar done) request
            (bordeaux-threads-2:with-lock-held (lock)
              (setf done t)
              (bordeaux-threads-2:condition-broadcast cvar))))
        ;; Free memory.
        (let ((local-allocations (aref allocations category))
              (local-pointers (aref pointers category)))
          (loop for index below (length local-allocations) do
            (setf (aref local-pointers index)
                  (free-memory
                   (aref local-allocations index)
                   (aref local-pointers index)))))))))

(defun worker-synchronize-and-invoke (denv invocations)
  (declare (denv denv) (list invocations))
  (barrier)
  (when (null (denv-serious-conditions denv))
    (handler-case
        (loop for invocation of-type invocation in invocations do
          ;; TODO
          (unless (shape-emptyp (invocation-iteration-space invocation))
            (funcall (invocation-kfn invocation)
                     (invocation-kernel invocation)
                     (invocation-iteration-space invocation)
                     (invocation-targets invocation)
                     (invocation-sources invocation)
                     denv)))
      (serious-condition (serious-condition)
        (bordeaux-threads-2:with-lock-held ((denv-serious-condition-lock denv))
          (push serious-condition (denv-serious-conditions denv)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sanity Checks

(defvar *simulator-step*)

(defstruct (simulated-memory
            (:constructor make-simulated-memory (allocation)))
  (allocation nil :type allocation :read-only t)
  ;; A hash table, mapping from indices to generations.
  (table (make-hash-table :test #'equal) :type hash-table)
  ;; The current generation.  Incremented by one whenever the access pattern
  ;; switches from reads to writes.
  (generation 0 :type unsigned-byte)
  ;; Whether the memory is currently being written to, or read from.
  (mode :w :type (member :r :w)))

(defun simulated-memory-read-index (simulated-memory index)
  (declare (simulated-memory simulated-memory) (list index))
  (with-slots (table generation mode) simulated-memory
    (when (eq mode :w) (setf mode :r))
    (multiple-value-bind (value presentp) (gethash index table)
      (if (not presentp)
          (error "Step ~D: Read from uninitialized memory at index ~A."
                 *simulator-step* index)
          (unless (<= value generation)
            (error "Step ~D: Read from wrongly initialized memory at index ~A."
                   *simulator-step* index))))))

(defun simulated-memory-read-shape (simulated-memory shape)
  (declare (simulated-memory simulated-memory) (shape shape))
  (map-shape
   (lambda (index)
     (simulated-memory-read-index simulated-memory index))
   shape))

(defun simulated-memory-write-index (simulated-memory index)
  (declare (simulated-memory simulated-memory) (list index))
  (with-slots (allocation table generation mode) simulated-memory
    (when (eq mode :r) (setf mode :w) (incf generation))
    (multiple-value-bind (value presentp) (gethash index table)
      (when presentp
        (unless (< value generation)
          (error "Two consecutive writes in step ~D to index ~A."
                 *simulator-step* index)))
      (setf (gethash index table) generation))))

(defun simulated-memory-write-shape (simulated-memory shape)
  (declare (simulated-memory simulated-memory) (shape shape))
  (map-shape
   (lambda (index)
     (simulated-memory-write-index simulated-memory index))
   shape))

(defun simulate-storage-reference (storage shape)
  (let* ((offset (storage-offset storage))
         (strides (storage-strides storage))
         (size (storage-size storage)))
    (assert (= (shape-rank shape) (length strides)))
    (map-shape
     (lambda (index)
       (let ((position
               (-
                (loop for index-component in index
                      for stride across strides
                      sum (* index-component stride))
                offset)))
         (assert (<= 0 position))
         (unless  (< position size)
           (error "~@<In step ~D, the index ~S references the position ~D ~
                   of an allocation with only ~D elements.~:@>"
                  *simulator-step* index position size))))
     shape)))

(defun simulate-cenv-evaluation (cenv)
  (declare (cenv cenv) (optimize (debug 3) (safety 3)))
  (with-slots (schedule result-shapes argument-shapes constant-arrays allocations) cenv
    (let ((memory-table (make-hash-table :test #'eq))
          (*simulator-step* -1))
      (labels
          ((simulated-memory (allocation)
             (alexandria:ensure-gethash
              allocation
              memory-table
              (make-simulated-memory allocation)))
           (invoke (invocation)
             (with-slots (kernel iteration-space sources targets) invocation
               (loop for source across sources for (buffer . stencils) in (kernel-sources kernel) do
                 (loop for stencil in stencils do
                   (loop for load-instruction in (stencil-instructions stencil) do
                     (let* ((transformation (load-instruction-transformation load-instruction))
                            (shape (transform-shape iteration-space transformation))
                            (memory (simulated-memory (storage-allocation source))))
                       (simulate-storage-reference source shape)
                       (simulated-memory-read-shape memory shape)))))
               (loop for target across targets for (buffer . stencils) in (kernel-targets kernel) do
                 (loop for stencil in stencils do
                   (loop for store-instruction in (stencil-instructions stencil) do
                     (let* ((transformation (store-instruction-transformation store-instruction))
                            (shape (transform-shape iteration-space transformation))
                            (memory (simulated-memory (storage-allocation target))))
                       (simulate-storage-reference target shape)
                       (simulated-memory-write-shape memory shape))))))))
        ;; Write all constants.
        (loop for allocation across (aref allocations +constant-allocation-category+) do
          (assert (= (allocation-category allocation) +constant-allocation-category+))
          (simulated-memory-write-shape
           (simulated-memory allocation)
           (array-shape (aref constant-arrays (allocation-color allocation)))))
        ;; Write all arguments.
        (loop for allocation across (aref allocations +argument-allocation-category+) do
          (assert (= (allocation-category allocation) +argument-allocation-category+))
          (simulated-memory-write-shape
           (simulated-memory allocation)
           (aref argument-shapes (allocation-color allocation))))
        ;; Process the schedule.
        (loop for actions in schedule for step from 0 do
          (setf *simulator-step* step)
          ;; Process all ghost layer copies.
          (loop for action across actions do
            (unless (not action)
              (mapc #'invoke (action-copy-invocations action))))
          ;; Process all work items.
          (loop for action across actions do
            (unless (not action)
              (mapc #'invoke (action-work-invocations action)))))
        ;; Check all results.
        (loop for allocation across (aref allocations +result-allocation-category+) do
          (assert (= (allocation-category allocation) +result-allocation-category+))
          (simulated-memory-read-shape
           (simulated-memory allocation)
           (aref result-shapes (allocation-color allocation))))))
    cenv))

(defun check-cenv (cenv)
  (simulate-cenv-evaluation cenv))
