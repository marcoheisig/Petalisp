;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defstruct (kernel
            (:copier nil)
            (:predicate kernel?))
  (recipe nil :type data-structure :read-only t)
  (cost 0 :type positive-integer :read-only t)
  (dependencies nil :type (vector kernel) :read-only nil)
  (finished? nil :type boolean :read-only nil))

(defun kernel-ready? (kernel)
  (every #'kernel-finished? (kernel-dependencies kernel)))

(define-evaluator local-evaluator
    (evaluate-kernels
     ((kernels (vector kernel)))))
