;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-computation (strided-array-constant)
  ((status :initform :scheduled
           :type (member :scheduled :allocated :finished :deleted)
           :accessor status)
   (lock :initform (make-lock))
   (cvar :initform (make-condition-variable)))
  (:documentation
   "A strided array constant under construction."))

(defmethod storage :around ((computation strided-array-computation))
  (with-lock-held ((lock computation))
    (loop :until (case (status computation)
                   (:finished t)
                   (:deleted
                    (simple-program-error "Reference to a deleted array.")))
          :do (condition-wait (cvar computation) (lock computation)))
    (call-next-method)))

(defun schedule (&rest objects)
  "Instruct Petalisp to compute all given OBJECTS asynchronously."
  (assert (every #'strided-array? objects))
  (when-let ((relevant-objects (delete-if #'immediate? objects)))
    (let* ((recipes (map 'vector #'shallow-copy relevant-objects))
           (targets (map 'vector (λ x (change-class x 'strided-array-computation))
                         relevant-objects)))
      (global-evaluator-evaluate-data-structures targets recipes)))
  (values))

(defun compute (&rest objects)
  "Return the computed values of all OBJECTS."
  (apply #'schedule objects)
  (apply #'values (mapcar #'storage objects)))

(define-evaluator global-evaluator
    (evaluate-data-structures
     ((targets (vector strided-array-computation))
      (recipes (vector data-structure)))
     (assert (= (length targets) (length recipes)))
     (let* ((kernels (kernelize recipes))
            (worklist (remove-if-not #'kernel-ready? kernels)))
       )))
