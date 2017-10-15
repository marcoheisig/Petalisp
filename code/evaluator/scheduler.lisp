;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-task-queue global-evaluator-thread)

(defun schedule-asynchronously (sequence)
  (assert (every #'strided-array? sequence))
  (when-let ((relevant-items (delete-if #'immediate? sequence)))
    (let* ((recipes (map 'vector #'shallow-copy relevant-items))
           (targets (map 'vector (λ x (change-class x 'intermediate-result))
                         relevant-items))
           (request (make-request)))
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
          (iterate (for item in-sequence (kernelize recipes))
                   (for index from 0)
                   (setf (storage (aref targets index))
                         (storage (evaluate item))))
          (complete request))))
      request)))

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
  intermediate-result)

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

