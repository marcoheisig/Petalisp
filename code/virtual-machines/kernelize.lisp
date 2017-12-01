;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; This is probably the most complicated part of Petalisp and deserves
;;; some explanation...
;;;
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
                (subtree-fragment-spaces root leaf-function))))
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

(defun subtree-fragment-spaces (root leaf-function)
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
  (multiple-value-bind (iteration-space sources)
      (subtree-iteration-space-and-sources root leaf-function index-space)
    ;; ITERATION-SPACE is currently based on the coordinate system of ROOT
    ;; and the subsequent reductions and is therefore -- from a virtual
    ;; machine perspective -- rather arbitrary. Unfortunately, all storage
    ;; references are relative to this space and therefore also
    ;; arbitrary. Since we want computationally equivalent kernels to have
    ;; the same blueprint, we need to normalize the iteration space.
    (multiple-value-bind (iteration-space transformations)
        (normalize-iteration-space
         iteration-space
         (subtree-fragment-transformations iteration-space transformations))
      (let ((blueprint (subtree-fragment-blueprint target root leaf-function sources transformations)))
        (make-instance 'kernel
          :target target
          :iteration-space iteration-space
          :sources sources
          :blueprint blueprint)))))

(defun subtree-iteration-space-and-sources (root leaf-function index-space)
  "Return as multiple values a vector of the ranges and a vector of the
  sources reachable from ROOT, as determined by the supplied
  LEAF-FUNCTION."
  (let ((sources (fvector))
        (ranges (copy-array (ranges (index-space root)) :fill-pointer t)))
    (labels
        ((traverse (node relevant-space)
           (when relevant-space
             (if-let ((leaf (funcall leaf-function node)))
               (fvector-pushnew leaf sources :test #'eq)
               (etypecase node
                 (application
                  (mapcar (λ input (traverse input relevant-space)) (inputs node)))
                 (reduction
                  (vector-push-extend (last-elt (ranges (index-space node))) ranges)
                  (traverse (input node) relevant-space))
                 (fusion
                  (let ((input (find relevant-space (inputs node)
                                     :key #'index-space
                                     :test #'intersection?)))
                    (traverse input (intersection relevant-space (index-space input)))))
                 (reference
                  (traverse (input node)
                            (intersection
                             (index-space (input node))
                             (funcall (transformation node) relevant-space)))))))))
      (traverse root index-space))
    (values (index-space ranges) sources)))

(defun normalize-iteration-space (iteration-space transformations)
  "Given an iteration space and a sequence of transformations, return a
  normalized iteration space and a sequence of adapted transformations."
  (flet ((dependent-ranges (transformation)
           (ranges
            (funcall transformation iteration-space))))
    (let ((iteration-ranges (ranges iteration-space))
          (dimension (dimension iteration-space)))
      (let ((gcd-vector (map 'vector #'range-step iteration-ranges))
            (min-vector (map 'vector #'range-start iteration-ranges)))
        ;; TODO there is quite some potential for optimization here,
        ;; e.g. via a MAP-OVER-TRANSFORMED-RANGES function
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
          (let ((normalizing-transformation
                  (affine-transformation input-constraints linear-operator min-vector)))
            (values
             (funcall (inverse normalizing-transformation) iteration-space)
             (flet ((normalize (transformation)
                      (composition transformation normalizing-transformation)))
               (map 'vector #'normalize  transformations)))))))))

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

(defun blueprint-range-information (ranges)
  (flet ((range-info (range)
           (let ((lb (log (size range) 2)))
             (ulist (expt (floor lb) 2)
                    (expt (ceiling lb) 2)
                    (range-step range)))))
    (map-ulist #'range-info ranges)))

(defun blueprint-storage-information (target sources)
  (flet ((storage-info (immediate)
           (element-type immediate)))
    (ulist* (storage-info target)
            (map-ulist #'storage-info sources))))

(defun blueprint-body (node leaf-function sources iteration-space transformation)
  (labels ((traverse (node relevant-space transformation)
             (if-let ((immediate (funcall leaf-function node)))
               (%reference (1+ (position immediate sources))
                           (blueprint-indices (composition (to-storage immediate) transformation)))
               (etypecase node
                 (reference
                  (traverse
                   (input node)
                   relevant-space
                   (composition (transformation node) transformation)))
                 (fusion
                  (traverse
                   (find-if (λ input (subspace? relevant-space (index-space input)))
                            (inputs node))
                   relevant-space
                   transformation))
                 (application
                  (%call (operator node)
                         (map-ulist (λ input (traverse input relevant-space transformation))
                                    (inputs node))))))))
    (traverse node iteration-space transformation)))

(defun subtree-fragment-blueprint (target root leaf-function iteration-space sources)
  (let ((dimension (dimension root)))
    (%blueprint
     (blueprint-range-information iteration-space)
     (blueprint-storage-information target sources)
     (funcall
      (named-lambda build-blueprint (range-id)
        (if (= range-id dimension)
            (%store (%reference 0 (blueprint-indices (identity-transformation dimension)))
                    (subgraph-blueprint-body
                     root leaf-function sources iteration-space (from-storage target)))
            (%for range-id (build-blueprint (1+ range-id)))))
      0))))
