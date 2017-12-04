;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; The goal is to translate a data flow graph into a graph of executable
;;; parts, called kernels. The data flow nodes form a directed acyclic
;;; graph. The graph is fully determined by a set of graph roots, typically
;;; the nodes passed to SCHEDULE.
;;;
;;; The result is a graph consisting only of immediate values, where each
;;; immediate is the target of one or more kernels. Each kernel describes
;;; how values of a subspace of the index space of its target can be
;;; computed. Furthermore each kernel tracks the set of its sources,
;;; i.e. those immediates that are referenced during their
;;; evaluation. Since the resulting immediate graph is used to decide a
;;; scheduling and allocation strategy, it is important that its kernels
;;; are easy to analyze, yet expressive enough to denote fast programs.
;;;
;;; The high level steps of the kernelization algorithm are as follows:
;;;
;;; 1. Determine the set of critical nodes, i.e. nodes that are referenced
;;;    more than once, are graph roots, are the input of a broadcasting
;;;    reference or have multiple inputs containing reductions. Each
;;;    critical node will later be the target of one or more kernels.
;;;
;;; 2. By construction, the nodes starting from one critical node, up to
;;;    and including the next critical nodes, form a tree. All fusion nodes
;;;    therein can be eliminated by determining a set of index spaces whose
;;;    union is the index space of the current critical node, but where
;;;    paths from each index space never pass through more than one input
;;;    of each fusion node.
;;;
;;; 3. For each index space from the previous step, create a suitable
;;;    kernel. To do so, the problem must be translated to a blueprint and
;;;    its iteration space must be determined.
;;;
;;; Once every critical node and index space thereof has been processed,
;;; the algorithm terminates.

(defun kernelize (graph-roots)
  "Translate the data flow graph specified by the given GRAPH-ROOTS to a
   graph of immediates and kernels. Return the roots of this new graph."
  (kernelize-subtrees
   (lambda (target root leaf-function)
     (setf (kernels target)
           (map 'vector
                (lambda (iteration-space)
                  (kernelize-subtree-fragment target root leaf-function iteration-space))
                (subtree-fragment-index-spaces root leaf-function))))
   graph-roots))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 1. Critical Nodes
;;;
;;; Critical nodes are the nodes that will later be allocated in main
;;; memory. All other nodes will only appear implicit as the part of a
;;; blueprint. The naive approach --- to treat each node as a critical node
;;; --- would lead to an insane number of allocations and memory
;;; traffic. Instead, the goal is to find a minimal set of critical nodes,
;;; while preserving some crucial properties.
;;;
;;; The criteria to determine critical nodes are:
;;;
;;; - An immediate is a critical node. This implies that all leaves of the
;;;   data flow graph are critical nodes. However such a critical node is
;;;   cheap, because it has already been allocated.
;;;
;;; - Each graph root is a critical node -- the whole purpose of Petalisp
;;;   is to compute the values of these graph roots.
;;;
;;; - A node that appears as an input of multiple other nodes of the same
;;;   graph, is a critical node. As a result, all non-critical nodes have
;;;   only a single user and --- as a consequence --- appear inside a tree,
;;;   whose root and leaves are critical nodes.
;;;
;;;   This tree property is a crucial prerequisite for blueprint creation,
;;;   scheduling and code generation.
;;;
;;; - The input of a broadcasting reference is a critical node. This
;;;   criterion is hardly obvious, but prevents a particular edge-case. If
;;;   broadcasting nodes would be allowed inside kernels, one could
;;;   construct a sequence of alternating reductions and broadcasts and
;;;   produce arbitrarily large kernels. On the other hand, the input of a
;;;   broadcasting reference is usually orders of magnitude smaller than
;;;   the output, so allocating it explicitly is hardly severe.
;;;
;;; - A node is a critical node, if it has more than one input path
;;;   containing a reduction node. This rather arcane criterion achieves,
;;;   that the iteration space of each kernel is an n-dimensional strided
;;;   cube, simplifying later analysis considerably. Introducing these
;;;   critical nodes is not terribly expensive, since the target of some
;;;   reductions is usually orders of magnitude smaller than their inputs.

(defun kernelize-subtrees (subtree-fn graph-roots)
  "Invoke SUBTREE-FN on each subtree in the graph spanned by the supplied
   GRAPH-ROOTS. For each subtree, SUBTREE-FN receives the following
   arguments:
   1. The target immediate
   2. The root of the tree in the data flow graph
   3. A function, mapping each tree leaf to its corresponding immediate

   Return the sequence of immediates corresponding to the GRAPH-ROOTS."
  (let ((critical-node-table (make-hash-table :test #'eq)))
    ;; Naively, CRITICAL-NODE-TABLE would simply contain an entry for each
    ;; critical node, mapping it to its corresponding immediate value. But
    ;; since there is initially some uncertainty about which nodes are
    ;; critical, the table will also contain an entry for each node with a
    ;; refcount of two or higher, but with a value of NIL. Furthermore,
    ;; immediates are not necessarily placed in the table, because they are
    ;; always critical and only map to themselves.
    (labels
        ((register-critical-node (node)
           (unless (gethash node critical-node-table)
             (setf (gethash node critical-node-table)
                   (corresponding-immediate node))
             (recurse-into node)
             (values nil)))
         (register-potentially-critical-node (node)
           (multiple-value-bind (value recurring)
               (gethash node critical-node-table)
             (unless value
               (cond
                 ((not recurring)
                  (setf (gethash node critical-node-table) nil)
                  (recurse-into node))
                 (recurring
                  (setf (gethash node critical-node-table)
                        (corresponding-immediate node))
                  (values nil))))))
         (recurse-into (node)
           (typecase node
             ;; deliberately ignore immediates
             (reduction (traverse (input node) nil))
             (reference (traverse (input node) nil)) ; TODO
             ((or application fusion)
              (let ((reductions
                      (loop for input in (inputs node)
                            count (traverse input nil))))
                (case reductions
                  (0 (values nil))
                  (1 (values t))
                  (otherwise (register-critical-node node)))))))
         (traverse (node critical?)
           (cond
             (critical?             (register-critical-node node))
             ((> (refcount node) 1) (register-potentially-critical-node node))
             (t                     (recurse-into node)))))
      (map nil #'register-critical-node graph-roots))
    ;; now call SUBTREE-FN for each subtree
    (flet ((process-hash-table-entry (tree-root target)
             (when (and target (not (eq tree-root target)))
               (flet ((leaf-function (node)
                        (cond
                          ;; the root is never a leaf
                          ((eq node tree-root) nil)
                          ;; all immediates are leaves
                          ((immediate? node) node)
                          ;; otherwise check the table
                          (t (values (gethash node critical-node-table))))))
                 (declare (dynamic-extent #'leaf-function))
                 (funcall subtree-fn target tree-root #'leaf-function)))))
      (maphash #'process-hash-table-entry critical-node-table)
      ;; finally return the result
      (flet ((lookup (node) (gethash node critical-node-table)))
        (map 'vector #'lookup graph-roots)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 2. Fusion Free Index Spaces
;;;
;;; The purpose of this step is to partition the index space of the root of
;;; a subtree into a set of subspaces, such that the tree fragment starting
;;; from this subspace is free of fusion nodes.

(defun subtree-fragment-index-spaces (root leaf-function)
  "Return a partitioning of the index space of ROOT, whose elements
   describe the maximal fusion-free paths through the subgraph from ROOT to
   some leaves, as determined by the supplied LEAF-FUNCTION."
  (labels
      ;; walk the tree starting from ROOT, up to the leaves as determined
      ;; by LEAF-FUNCTION. RELEVANT-SPACE is a subspace of the index space
      ;; of ROOT, that shrinks when selecting a particular input of a
      ;; fusion node. TRANSFORMATION as a mapping between the coordinate
      ;; system of the current node and the coordinate system of the root
      ;; node.
      ((fragment-spaces (node relevant-space transformation)
         ;; NODE is the root of the current subtree. FRAGMENT-SPACES
         ;; returns a list of all iteration spaces in the current subtree,
         ;; or NIL, if the subtree contains no fusions.
         (unless (funcall leaf-function node)
           (typecase node
             (fusion
              (iterate
                (for input in (inputs node))
                (when-let ((subspace (intersection relevant-space (index-space input))))
                  (nconcing
                   (or (fragment-spaces input subspace transformation)
                       (list (funcall (inverse transformation) subspace)))))))
             (reference
              (when-let ((subspace (intersection relevant-space (index-space node))))
                (fragment-spaces (input node) subspace
                                  (composition (transformation node) transformation))))
             (reduction
              (fragment-spaces (input node) relevant-space transformation))
             (application
              (let* ((number-of-fusing-subtrees 0)
                   (index-spaces
                     (iterate
                       (for input in (inputs node))
                       (when-let ((spaces (fragment-spaces input relevant-space transformation)))
                         (incf number-of-fusing-subtrees)
                         (nconcing spaces)))))
              (if (> number-of-fusing-subtrees 1)
                  (subdivision index-spaces)
                  index-spaces)))))))
    (or
     (fragment-spaces root (index-space root) (identity-transformation (dimension root)))
     (list (index-space root)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 3. Kernel Creation
;;;
;;; Several exciting things happen during kernel creation:
;;;
;;; - reference nodes are lifted and combined, until they reach the
;;;   leaves. As a consequence, the body of a recipe is now free of
;;;   references and fusions
;;;
;;; - a common iteration space is derived
;;;
;;; - for each memory reference, a mapping from the iteration space to the
;;;   storage coordinates of the referenced immediate is determined
;;;
;;; The iteration space of a kernel is an N-dimensional strided cube.

(defun kernelize-subtree-fragment (target root leaf-function index-space)
  "Return the kernel that computes the INDEX-SPACE of TARGET, according
   to the data flow graph prescribed by ROOT and LEAF-FUNCTION."
  (with-subtree-fragment-information (root leaf-function index-space)
      (iteration-space sources source-ids transformations body)
    ;; ITERATION-SPACE is currently based on the coordinate system of ROOT
    ;; and the subsequent reductions and is therefore -- from a virtual
    ;; machine perspective -- rather arbitrary. Unfortunately, all storage
    ;; references are relative to this space and therefore also
    ;; arbitrary. Since we want computationally equivalent kernels to have
    ;; the same blueprint, we need to normalize the iteration space.
    #+nil(let ((normalization (iteration-space-normalization iteration-space transformations))))
    (make-instance 'kernel
      :target target
      :iteration-space iteration-space
      ;;:unknown-operators unknown-operators
      :sources sources
      :blueprint
      (flet ((range-info (range)
               (let ((lb (log (size range) 2)))
                 (ulist (expt (floor lb) 2)
                        (expt (ceiling lb) 2)
                        (range-step range))))
             (memory-reference-info (source-id transformation)
               (ulist* source-id (blueprint-indices transformation)))
             (storage-info (immediate)
               (element-type immediate)))
        (ulist 'blueprint
               (map-ulist #'range-info (ranges iteration-space))
               (map-ulist #'memory-reference-info source-ids transformations)
               (ulist (storage-info target))
               (map-ulist #'storage-info sources)
               body)))))

(defmacro with-subtree-fragment-information
    ((root leaf-function index-space) (&rest keyword-arguments) &body body)
  `(call-with-subtree-fragment-information
    ,root ,leaf-function ,index-space
    (lambda (&key ,@keyword-arguments)
      ,@body)))

(defun call-with-subtree-fragment-information
    (root leaf-function index-space continuation)
  "Return as multiple values
   1. the iteration space, including all reductions
   2. a vector of the referenced sources
   3. a vector of source ids
   4. a vector of leaf transformations, one for each source id
   5. the body of the blueprint"
  (let ((sources (fvector))
        (ranges (copy-array (ranges (index-space root)) :fill-pointer t))
        (source-ids (fvector))
        (transformations (fvector)))
    (labels
        ((traverse (node relevant-space transformation)
           (when relevant-space
             (if-let ((leaf (funcall leaf-function node)))
               ;; register leaf nodes and return their id
               (let ((source-id
                       (or (position leaf sources :test #'eq)
                           (vector-push-extend leaf sources)))
                     (transformation
                       (composition (to-storage leaf) transformation)))
                 (vector-push-extend source-id source-ids)
                 (vector-push-extend transformation transformations))
               (etypecase node
                 ;; translate applications
                 (application
                  (flet ((traverse-input (input)
                           (traverse input relevant-space transformation)))
                    (ulist* 'call (operator node) (map-ulist #'traverse-input (inputs node)))))
                 ;; increase the iteration space on each reduction
                 (reduction
                  (vector-push-extend (last-elt (ranges (index-space node))) ranges)
                  (flet ((traverse (input)
                           (traverse input relevant-space (enlarge-transformation transformation))))
                    (ulist 'reduce (operator node) (traverse (input node)))))
                 ;; eliminate fusions
                 (fusion
                  (let* ((input (find relevant-space (inputs node)
                                      :key #'index-space
                                      :test #'intersection?))
                         (relevant-space (intersection relevant-space (index-space input))))
                    (traverse input relevant-space transformation)))
                 ;; eliminate references, but adapt the currently relevant
                 ;; space and transformation
                 (reference
                  (let ((relevant-space
                          (intersection
                           (index-space (input node))
                           (funcall (transformation node) relevant-space)))
                        (transformation
                          (composition (transformation node) transformation)))
                    (traverse (input node) relevant-space transformation))))))))
      (let ((body (traverse root index-space (identity-transformation (dimension root)))))
        (funcall
         continuation
         :index-space (index-space ranges)
         :sources sources
         :source-ids source-ids
         :transformations transformations
         :body body)))))

(defun iteration-space-normalization (iteration-space transformations)
  (flet ((dependent-ranges (transformation)
           (ranges
            (funcall transformation iteration-space))))
    (let ((iteration-ranges (ranges iteration-space))
          (dimension (dimension iteration-space)))
      (let ((gcd-vector (map 'vector #'range-step iteration-ranges))
            (min-vector (map 'vector #'range-start iteration-ranges)))
        ;; TODO there is quite some potential for optimization here,
        ;; e.g. via a MAP-OVER-TRANSFORMED-RANGES function to reduce
        ;; consing
        (loop for transformation across transformations do
          (loop for range across (dependent-ranges transformation)
                and i from 0 do
                  (setf (aref gcd-vector i)
                        (gcd (aref gcd-vector i)
                             (range-step range)))
                  (setf (aref  min-vector i)
                        (min (aref min-vector i)
                             (range-start range)))))
        (let ((input-constraints (make-array dimension :initial-element nil))
              (linear-operator
                (let ((column-indices (make-array dimension)))
                  (loop for i below (length column-indices) do
                    (setf (aref column-indices i) i))
                  (scaled-permutation-matrix dimension dimension column-indices gcd-vector))))
          (affine-transformation input-constraints linear-operator min-vector))))))

(defgeneric blueprint-indices (transformation)
  (:method ((transformation identity-transformation))
    (let ((dimension (input-dimension transformation)))
      (let (result)
        (iterate
          (for index from (1- dimension) downto 0)
          (setf result (ulist* (ulist index 1 0) result)))
        result)))
  (:method ((transformation affine-transformation))
    (let (result)
      (iterate
        (for column in-vector (spm-column-indices (linear-operator transformation)) downto 0)
        (for value in-vector (spm-values (linear-operator transformation)) downto 0)
        (for offset in-vector (translation-vector transformation) downto 0)
        (setf result (ulist* (ulist column value offset) result)))
      result)))
