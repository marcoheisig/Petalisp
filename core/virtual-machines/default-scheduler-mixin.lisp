;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/virtual-machines/default-scheduler-mixin
  (:use :closer-common-lisp :alexandria :iterate)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/all
   :petalisp/core/kernel-creation/all
   :petalisp/core/virtual-machines/virtual-machine)
  (:export
   #:default-scheduler-mixin
   #:vm/bind-memory
   #:vm/compile
   #:vm/compute
   #:vm/execute
   #:vm/free-memory))

(in-package :petalisp/core/virtual-machines/default-scheduler-mixin)

(define-class default-scheduler-mixin ()
  ((scheduler-queue :type queue :initform (make-queue))
   (scheduler-thread :initform nil)))

(defgeneric vm/bind-memory (virtual-machine immediate)
  (:documentation
   "Instruct VIRTUAL-MACHINE to suitably set the STORAGE slot of
IMMEDIATE."))

(defgeneric vm/compile (virtual-machine blueprint)
  (:documentation
   "Instruct VIRTUAL-MACHINE to prepare the given BLUEPRINT for execution."))

(defgeneric vm/compute (virtual-machine graph-roots)
  (:documentation
   "Instruct VIRTUAL-MACHINE to compute the sequence of data structures
GRAPH-ROOTS. Return the computed values of all GRAPH-ROOTS."))

(defgeneric vm/execute (virtual-machine kernel)
  (:documentation
   "Instruct VIRTUAL-MACHINE to execute the given KERNEL, assuming that all
its sources and targets have already been allocated and computed."))

(defgeneric vm/free-memory (virtual-machine immediate)
  (:documentation
   "Instruct VIRTUAL-MACHINE to reclaim the STORAGE of IMMEDIATE and set
the STORAGE slot of IMMEDIATE to NIL."))

(defmethod vm/schedule ((vm default-scheduler-mixin) targets recipes)
  (let ((request (make-request)))
    ;; TODO currently schedules synchronously for easier debugging
    (loop for immediate across (kernelize recipes)
          for index from 0 do
            (setf (storage (aref targets index))
                  (storage (evaluate-naively vm immediate))))
    (complete request)
    #+nil
    (prog1 request
      (run-in-global-evaluator-thread
       (λ (%schedule virtual-machine targets blueprints request))))))

(defun evaluate-naively (vm immediate)
  ;; only evaluate once
  (unless (storage immediate)
    ;; evaluate all dependencies
    (let (dependencies)
      (loop for kernel across (kernels immediate) do
        (loop for index from 1 below (length (kernel-references kernel))
              for source = (aref (kernel-references kernel) index) do
                (pushnew source dependencies)))
      (map nil (lambda (dependency) (evaluate-naively vm dependency))
           dependencies))
    ;; allocate memory
    (vm/bind-memory vm immediate)
    ;; compute all kernels
    (loop for kernel across (kernels immediate) do
      (vm/execute vm kernel)
      (loop for index from 1 below (length (kernel-references kernel))
            for source = (aref (kernel-references kernel) index) do
              (when (zerop (decf (refcount source)))
                (vm/free-memory vm source)))))
  immediate)
