;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defvar *memory-pool* nil)

(defun bind-memory (kernel-target)
  (let ((array-dimensions
          (mapcar #'size (ranges (index-space kernel-target))))
        (element-type (element-type kernel-target)))
    (setf (storage kernel-target)
          (or (find-if
               (λ array
                  (and
                   (equal (array-dimensions array) array-dimensions)
                   (type= (array-element-type array) element-type)))
               *memory-pool*)
              (make-array array-dimensions :element-type element-type)))))

(defun free-memory (kernel-target)
  (push (storage kernel-target) *memory-pool*)
  (setf (storage kernel-target) nil))

(defun evaluate-kernel-target (kernel-target)
  ;; allocate memory
  (bind-memory kernel-target)
  (setf (refcount kernel-target)
        (count-if #'kernel-target? (users kernel-target)))
  ;; compute all fragments
  (iterate
    (for kernel-fragment in-vector (kernel-fragments kernel-target))
    (evaluate-kernel-fragment kernel-fragment)
    ;; potentially free predecessor memory
    (iterate
      (for binding in-vector (bindings kernel-fragment))
      (when (kernel-target? binding)
        (when (= 0 (decf (refcount binding)))
          (free-memory binding))))))

(define-evaluator global-evaluator
    (evaluate-data-structures
     ((targets (vector strided-array-computation))
      (recipes (vector data-structure)))
     (assert (= (length targets) (length recipes)))
     (labels ((evaluate (kernel-target)
                (let ((dependencies (remove-if-not (compose #'null #'storage)
                                                   (users kernel-target))))
                  (mapc #'evaluate dependencies)
                  (evaluate-kernel-target kernel-target))))
       (map nil #'evaluate (kernelize recipes)))))
