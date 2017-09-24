;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-evaluator global-evaluator
    (evaluate-data-structures
     ((data-structures (vector data-structure)))
     (print data-structures)))

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
      (map nil #'populate-use-table graph-roots)
      ;; the kernel table maps from data structures to kernels
      (let ((kernel-table (make-hash-table :test #'eq)))
        (labels ((kernelize-recipe (node)
                   (let (dependencies)
                     (labels ((recurse (node)
                                (if (leaf? node)
                                    (push node dependencies)
                                    (mapc #'dependencies (inputs node)))))
                       (recurse node))))
                 (already-visited? (node) (gethash node kernel-table))
                 (inner-node? (node))
                 (recurse (node)
                   (unless (leaf? node)
                     (if (inner-node? node)
                         (let ((recipe (shallow-copy node)))
                           (mapc #'recurse (inputs recipe)))
                         (mapc #'recurse (inputs node))))))
          (map nil #'recurse graph-roots)
          (map 'vector
               #'(lambda (x)
                   (make-instance 'kernel :recipe x))
               graph-roots))))))
