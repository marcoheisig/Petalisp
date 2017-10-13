;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-task-queue global-evaluator-thread)

(defun schedule-items (list-of-items)
  (assert (every #'strided-array? list-of-items))
  (when-let ((relevant-items (delete-if #'immediate? list-of-items)))
    (let* ((recipes (map 'vector #'shallow-copy relevant-items))
           (targets (map 'vector (λ x (change-class x 'intermediate-result))
                         relevant-items)))
      (run-in-global-evaluator-thread
       (λ
        (labels ((evaluate (intermediate-result)
                   (iterate
                     (for kernel in (kernels intermediate-result))
                     (iterate (for binding in-vector (bindings kernel))
                              (when (and (intermediate-result? binding)
                                         (not (storage binding)))
                                (evaluate binding))))
                   (evaluate-intermediate-result intermediate-result)))
          ;;(graphviz-draw-graph 'data-flow-graph (kernelize recipes))
          (map nil #'evaluate (kernelize recipes))))))))

(defun evaluate-intermediate-result (intermediate-result)
  ;; allocate memory
  (bind-memory intermediate-result)
  (setf (refcount intermediate-result)
        (count-if #'intermediate-result? (users intermediate-result)))
  ;; compute all kernels
  (iterate
    (for kernel in (kernels intermediate-result))
    (evaluate-kernel kernel)
    ;; potentially free predecessor memory
    (iterate
      (for binding in-vector (bindings kernel))
      (when (intermediate-result? binding)
        (when (= 0 (decf (refcount binding)))
          (free-memory binding)))))
  #+nil
  (print (storage intermediate-result)))

(defun evaluate-kernel (kernel)
  (let* ((binding-symbols
           (iterate (for index below (length (bindings kernel)))
                    (collect (binding-symbol index))))
         (form
           `(lambda (target ,@binding-symbols)
              (%for ,(ranges
                      (funcall
                       (inverse (zero-based-transformation (target kernel)))
                       (index-space kernel)))
                  ,(recipe kernel)))))
    (apply (compile nil form)
           (storage (target kernel))
           (map 'list #'storage (bindings kernel)))))

