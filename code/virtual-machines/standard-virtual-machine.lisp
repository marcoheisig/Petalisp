;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class standard-virtual-machine (virtual-machine)
  ((compile-cache :type hash-table :initform (make-hash-table :test #'eq))
   (memory-pool :type hash-table :initform (make-hash-table :test #'equalp))
   (scheduler-queue :type queue :initform (make-queue))
   (scheduler-thread :initform nil)))

(defmethod vm/schedule ((vm standard-virtual-machine) graph-roots)
  (let* ((graph-roots (map 'vector #'shallow-copy graph-roots))
         (targets (map 'vector (λ x (change-class x 'strided-array-immediate)) graph-roots))
         (request (make-request)))
    ;; TODO currently schedules synchronously for easier debugging
    (iterate (for immediate in-sequence (kernelize-graph graph-roots))
             (for index from 0)
             (setf (storage (aref targets index))
                   (storage (evaluate-naively vm immediate))))
    (complete request)
    #+nil
    (prog1 request
      (run-in-global-evaluator-thread
       (λ (%schedule virtual-machine targets blueprints request))))))
