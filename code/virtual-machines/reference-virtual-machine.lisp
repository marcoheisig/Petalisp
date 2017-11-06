;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class reference-virtual-machine (virtual-machine)
  ((evaluated-nodes :type hash-table :initform (make-hash-table :test #'eq))))

(defmethod vm/schedule ((vm reference-virtual-machine) graph-roots)
  (let* ((target-graphs
           (map 'vector
                (lambda (root)
                  (etypecase root
                    (immediate nil)
                    (data-structure (shallow-copy root))))
                graph-roots))
         (targets
           (map 'vector
                (lambda (root)
                  (etypecase root
                    (immediate root)
                    (strided-array (change-class root 'strided-array-immediate))))
                graph-roots))
         (request (make-request)))
    (iterate
      (for target in-sequence targets)
      (for target-graph in-sequence target-graphs)
      (setf (storage target)
            (storage (reference-vm/evaluate-node vm target-graph))))
    (clrhash (evaluated-nodes vm))
    (complete request)))

(defgeneric reference-vm/evaluate-node (virtual-machine node)
  (:method :around ((vm reference-virtual-machine) (node data-structure))
    (with-hash-table-memoization (node)
        (evaluated-nodes vm)
      (call-next-method)))
  (:method ((vm reference-virtual-machine) (node immediate))
    node))

(defmethod reference-vm/evaluate-node
    ((vm reference-virtual-machine) (node application))
  (let ((result (corresponding-immediate node)))
    ))

(defmethod reference-vm/evaluate-node
    ((vm reference-virtual-machine) (node reduction))
  (let ((result (corresponding-immediate node)))
    ))

(defmethod reference-vm/evaluate-node
    ((vm reference-virtual-machine) (node fusion))
    (let ((result (corresponding-immediate node)))
      ))

(defmethod reference-vm/evaluate-node
    ((vm reference-virtual-machine) (node reference))
    (let ((result (corresponding-immediate node)))
      ))
