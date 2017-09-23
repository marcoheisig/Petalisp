;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-evaluator global-evaluator
    (evaluate-data-structures
     ((data-structures (vector data-structure)))
     (iterate (for kernels
                   initially (kernelize data-structures)
                   then (remove-if-not #'kernel-ready? kernels))
              (for ready-kernels = (remove-if #'kernel-ready? kernels))
              (if (emptyp ready-kernels)
                  (finish)
                  (local-evaluator-evaluate-kernels ready-kernels)))))

(defun kernelize (graph-roots)
  "Return a list of kernels whose evaluation is equivalent to the
evaluation of the given GRAPH-ROOTS."
  ;; the use table maps from each node to the list of its users.
  (let ((use-table (make-hash-table :test #'eq)))
    (labels ((users (node)
               (gethash node use-table))
             ((setf users) (node value)
               (setf (gethash node use-table) value))
             (add (user producer)
               (if (users producer)
                   (push user (users producer))
                   (setf (users producer) (list user))))
             (populate-use-table (node)
               (when-let ((producers (inputs node)))
                 ;; check whether node is visited for the first time
                 (unless (member node (users (first producers)))
                   (dolist (producer producers) (add node producer))))))
      (map nil #'populate-use-table graph-roots))
    ;; cut the graph roots into kernels, i.e. graphs where every node has
    ;; at most one user
    (labels ((dependencies (node)
               (let (dependencies)
                 (labels ((recurse (node)
                            (if (leaf? node)
                                (push node dependencies)
                                (mapc #'dependencies (inputs node)))))
                   (recurse node))))
             (recurse (node)
               (unless (leaf? node)
                 (if (or (< 1 (length (gethash node use-table)))
                         (member node graph-roots :test #'eq))
                     (let ((recipe (shallow-copy node)))
                       (mapc #'recurse (inputs recipe))
                       (change-class node 'strided-array-elaboration
                                     :dependencies (dependencies recipe)
                                     :recipe recipe))
                     (mapc #'recurse (inputs node))))))
      (map nil #'recurse graph-roots)
      (map 'vector
           #'(lambda (x) (make-kernel :recipe x :dependencies #() :cost 1))
           graph-roots))))
