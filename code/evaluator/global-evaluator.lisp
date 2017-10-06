;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun schedule (&rest objects)
  "Instruct Petalisp to compute all given OBJECTS asynchronously."
  (assert (every #'strided-array? objects))
  (when-let ((relevant-objects (delete-if #'immediate? objects)))
    (let* ((recipes (map 'vector #'shallow-copy
                         relevant-objects))
           (targets (map 'vector (λ x (change-class x 'strided-array-computation))
                         relevant-objects)))
      (global-evaluator-evaluate-data-structures targets recipes))))

(defun compute (&rest objects)
  "Return the computed values of all OBJECTS."
  (apply #'schedule objects)
  (apply #'values (mapcar #'depetalispify objects)))

(define-evaluator global-evaluator
    (evaluate-data-structures
     ((targets (vector strided-array-computation))
      (recipes (vector data-structure)))
     (assert (= (length targets) (length recipes)))
     (let ((target-kernels (kernelize recipes))
           memory-pool) ; a list of arrays
       (flet ((bind-memory (kernel-target)
                (let ((array-dimensions
                        (mapcar #'size (ranges (index-space kernel-target))))
                      (element-type (element-type kernel-target)))
                  (setf (storage kernel-target)
                        (or (find-if
                             (λ array
                                (and
                                 (equal (array-dimensions array) array-dimensions)
                                 (type= (array-element-type array) element-type)))
                             memory-pool)
                            (make-array array-dimensions :element-type element-type)))))
              (free-memory (kernel-target)
                (push (storage kernel-target) memory-pool)
                (setf (storage kernel-target) nil)))
         ;; TODO we assume every kernel has exactly one user. This is true
         ;; only for simple stencil codes, but enough for testing
         (labels
             ((evaluate-kernel-fragment (kernel-fragment)
                kernel-fragment)
              (evaluate-kernel-target (kernel-target &optional unique-user)
                (iterate
                  (initially (bind-memory kernel-target))
                  (for kernel-fragment in-vector (kernel-fragments kernel-target))
                  (evaluate-kernel-fragment kernel-fragment)
                  (when unique-user (free-memory unique-user))))
              (compute (kernel-target)
                (let ((users (users kernel-target)))
                  (case (length users)
                    (0 (evaluate-kernel-target kernel-target))
                    (1 (compute (first users)))
                    (2 (error "Too many users.")))))))))))
