(in-package #:petalisp.native-backend)

(defstruct (allocation
            (:predicate allocationp)
            (:constructor make-allocation))
  ;; How many bytes of layout must be assigned to the allocation eventually.
  (size-in-bytes nil :type unsigned-byte :read-only t)
  ;; An unsigned integer with the following meaning:
  ;;
  ;; 0 - The allocation uses the memory of an existing, immutable array.
  ;;
  ;; 1 - The allocation uses the memory of a result array.
  ;;
  ;; 2 - The allocation uses the memory of an argument array.
  ;;
  ;; N - The allocation is allocated by the worker whose id is N-3.
  (category nil :type unsigned-byte)
  ;; Whether the allocation manages elements that are unboxed and don't have to
  ;; be tracked by the GC.
  (unboxed nil :type boolean)
  ;; An unsigned integer that is chosen such that, for a particular schedule,
  ;; all allocations with the same size and color can share one memory region.
  (color nil :type unsigned-byte :read-only t))

(defconstant +constant-allocation-category+ 0)

(defconstant +result-allocation-category+ 1)

(defconstant +argument-allocation-category+ 2)

(defconstant +worker-allocation-category-offset+ 3)

(defun compute-allocations (schedule primogenitor-buffer-shard-vector unknowns backend)
  "Returns a vector of vectors of allocations, and ensures that each layout
being referenced by the supplied schedule has its allocation slot set to one of
these allocations.  Returns a second value that is a vector of all constant
arrays that were referenced in the schedule."
  (let* ((program (buffer-program (buffer-shard-buffer (aref primogenitor-buffer-shard-vector 0))))
         (nworkers (worker-pool-size (backend-worker-pool backend)))
         (ncategories (+ nworkers +worker-allocation-category-offset+))
         (nbuckets 40)
         (reversed-constant-array-list '())
         (unboxed-allocation-lists (make-array (list ncategories nbuckets) :initial-element '()))
         (boxed-allocation-lists (make-array (list ncategories nbuckets) :initial-element '()))
         (allocation-colors (make-array ncategories :initial-element 0))
         (layout-counter-table (make-hash-table)))
    (labels ((incf-layout-counter (layout)
               (incf (gethash layout layout-counter-table 0)))
             (decf-layout-counter (layout)
               (decf (gethash layout layout-counter-table)))
             (buffer-primogenitor-buffer-shard (buffer)
               (aref primogenitor-buffer-shard-vector (buffer-number buffer)))
             (push-allocation (allocation)
               (declare (allocation allocation))
               (with-slots (size-in-bytes category unboxed) allocation
                 (let ((bucket (max 0 (1- (integer-length (1- size-in-bytes))))))
                   (if unboxed
                       (push allocation (aref unboxed-allocation-lists category bucket))
                       (push allocation (aref boxed-allocation-lists category bucket))))))
             (pop-allocation (category layout)
               (let* ((size-in-bytes (layout-size-in-bytes layout))
                      (bucket (max 0 (1- (integer-length (1- size-in-bytes)))))
                      (unboxed (ntype-unboxed-p (layout-ntype layout)))
                      (allocation-lists (if unboxed unboxed-allocation-lists boxed-allocation-lists)))
                 (or (pop (aref allocation-lists category bucket))
                     (prog1 (make-allocation
                             :size-in-bytes (expt 2 (1+ bucket))
                             :category category
                             :unboxed unboxed
                             :color (next-color category))))))
             (next-color (category)
               (prog1 (aref allocation-colors category)
                 (incf (aref allocation-colors category)))))
      ;; Process all constants.
      (loop for (buffer . lazy-array) in (program-leaf-alist program) do
        (let ((delayed-action (lazy-array-delayed-action lazy-array)))
          (when (delayed-array-p delayed-action)
            (let* ((array (delayed-array-storage delayed-action))
                   (allocation
                     (make-allocation
                      :size-in-bytes (array-size-in-bytes array)
                      :category +constant-allocation-category+
                      :unboxed (eq (array-element-type array) 't)
                      :color (next-color +constant-allocation-category+))))
              (buffer-shard-bind (buffer-primogenitor-buffer-shard buffer) allocation)
              (push array reversed-constant-array-list)
              (push-allocation allocation)))))
      ;; Process all results.
      (loop for root-buffer in (program-root-buffers program) do
        (let ((allocation
                (make-allocation
                 :size-in-bytes (buffer-size-in-bytes root-buffer)
                 :category +result-allocation-category+
                 :unboxed (ntype-unboxed-p (buffer-ntype root-buffer))
                 :color (next-color +result-allocation-category+))))
          (buffer-shard-bind (buffer-primogenitor-buffer-shard root-buffer) allocation)
          (push-allocation allocation)))
      ;; Process all arguments.
      (loop for unknown in unknowns do
        (let* ((entry (rassoc unknown (program-leaf-alist program)))
               (allocation
                 (make-allocation
                  :size-in-bytes (lazy-array-size-in-bytes unknown)
                  :category +argument-allocation-category+
                  :unboxed (ntype-unboxed-p (lazy-array-ntype unknown))
                  :color (next-color +argument-allocation-category+))))
          (when entry
            (buffer-shard-bind (buffer-primogenitor-buffer-shard (car entry)) allocation))
          (push-allocation allocation)))
      ;; Traverse the schedule once, and count how often each piece of layout
      ;; is read from.
      (loop for action-vector in schedule do
        (loop for action across action-vector do
          (unless (not action)
            (loop for invocation in (action-copy-invocations action) do
              (loop for layout across (invocation-sources invocation) do
                (incf-layout-counter layout)))
            (loop for invocation in (action-work-invocations action) do
              (loop for layout across (invocation-sources invocation) do
                (incf-layout-counter layout))))))
      ;; Traverse the schedule again, and assign each layout an allocation.
      (loop for action-vector in schedule do
        ;; Free memory that becomes available after copying the ghost layers.
        (loop for action across action-vector for category from +worker-allocation-category-offset+ do
          (unless (not action)
            (loop for invocation in (action-copy-invocations action) do
              (loop for layout across (invocation-sources invocation) do
                (when (zerop (decf-layout-counter layout))
                  (push-allocation (layout-allocation layout)))))))
        ;; Ensure that each target of each action is allocated.
        (loop for action across action-vector for category from +worker-allocation-category-offset+ do
          (unless (not action)
            (loop for invocation in (action-work-invocations action) do
              (loop for layout across (invocation-targets invocation) do
                (unless (layout-allocation layout)
                  (let ((allocation (pop-allocation category layout)))
                    (layout-bind layout allocation)
                    #+(or)
                    (when (zerop (gethash layout layout-counter-table 0))
                      #+(or)(break "~A ~A" layout allocation) ;; TODO
                      (push-allocation allocation))))))))
        ;; Ensure that sources that are no longer used after this action are
        ;; freed.
        (loop for action across action-vector for category from +worker-allocation-category-offset+ do
          (unless (not action)
            (loop for invocation in (action-work-invocations action) do
              (loop for layout across (invocation-sources invocation) do
                (when (zerop (decf-layout-counter layout))
                  (push-allocation (layout-allocation layout))))))))
      (values
       ;; Create the vector of per-category-allocations.
       (let ((allocations (make-array ncategories)))
         (loop for category below ncategories do
           (let* ((ncolors (aref allocation-colors category))
                  (vector (make-array ncolors)))
             (loop for bucket below nbuckets do
               (loop for allocation in (aref unboxed-allocation-lists category bucket) do
                 (assert (= (allocation-category allocation) category))
                 (setf (aref vector (allocation-color allocation))
                       allocation))
               (loop for allocation in (aref boxed-allocation-lists category bucket) do
                 (assert (= (allocation-category allocation) category))
                 (setf (aref vector (allocation-color allocation))
                       allocation)))
             (setf (aref allocations category)
                   vector)))
         ;; Ensure that each vector of allocations is well formed.
         (loop for per-category-allocations across allocations do
           (loop for allocation across per-category-allocations do
             (assert (allocationp allocation))))
         allocations)
       ;; Create the vector of constant arrays.
       (let* ((nconstants (length reversed-constant-array-list))
              (constant-arrays (make-array nconstants)))
         (loop for index from (1- nconstants) downto 0
               for constant-array in reversed-constant-array-list
               do (setf (aref constant-arrays index)
                        constant-array))
         constant-arrays)))))

(defun buffer-shard-bind (buffer-shard allocation)
  (layout-bind (buffer-shard-layout buffer-shard) allocation))

(defun layout-bind (layout allocation)
  (assert (not (layout-allocation layout)))
  (assert (<= (layout-size-in-bytes layout)
              (allocation-size-in-bytes allocation)))
  (setf (layout-allocation layout) allocation))

(defun ntype-unboxed-p (ntype)
  (not (typo:ntype=
        (typo:upgraded-array-element-ntype ntype)
        (typo:universal-ntype))))

(defun array-size-in-bytes (array)
  (bytes-from-bits
   (* (array-total-size array)
      (ntype-bits-per-element
       (typo:array-element-ntype array)))))

(defun buffer-size-in-bytes (buffer)
  (bytes-from-bits
   (* (buffer-size buffer)
      (ntype-bits-per-element
       (buffer-ntype buffer)))))

(defun lazy-array-size-in-bytes (lazy-array)
  (bytes-from-bits
   (* (lazy-array-size lazy-array)
      (ntype-bits-per-element
       (lazy-array-ntype lazy-array)))))

(defun layout-size-in-bytes (layout)
  (bytes-from-bits
   (* (layout-size layout)
      (ntype-bits-per-element
       (layout-ntype layout)))))

(defun bytes-from-bits (bits)
  (values (ceiling bits 8)))

(defun ntype-bits-per-element (ntype)
  (declare (typo:ntype ntype))
  (petalisp.utilities:clp2
   (typo:ntype-bits ntype)))

(defun allocate-memory (allocation)
  (declare (allocation allocation))
  (if (allocation-unboxed allocation)
      (cffi:foreign-alloc
       :uint8
       :count (allocation-size-in-bytes allocation))
      (make-array (allocation-size-in-bytes allocation))))

(defun free-memory (allocation memory)
  (declare (allocation allocation))
  (if (allocation-unboxed allocation)
      (cffi:foreign-free memory)
      (values)))
