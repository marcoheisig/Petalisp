;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class virtual-machine ()
  ((compile-cache :type hash-table :initform (make-hash-table :test #'eq))
   (memory-pool :type hash-table :initform (make-hash-table :test #'equalp))
   (scheduler-queue :type queue :initform (make-queue))
   (scheduler-thread :initform nil))
  (:documentation
   "A virtual machine is an abstraction over a set of hardware
   resources. All handling of kernels --- such as performance analysis,
   compilation and execution --- is done in the context of a particular
   virtual machine."))

(defmethod vm/schedule :before ((vm virtual-machine) (graph-roots sequence))
  (assert (every #'data-structure? graph-roots)))

(defmethod vm/schedule ((vm virtual-machine) graph-roots)
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
    ;; TODO currently schedules synchronously for easier debugging
    (iterate (for immediate in-sequence (kernelize-graph target-graphs))
             (for index from 0)
             (setf (storage (aref targets index))
                   (storage (evaluate-naively vm immediate))))
    (complete request)
    #+nil
    (prog1 request
      (run-in-global-evaluator-thread
       (λ (%schedule virtual-machine targets blueprints request))))))
