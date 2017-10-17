;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun evaluate-kernel (kernel)
  (let* ((binding-symbols
           (iterate (for index below (length (sources kernel)))
                    (collect (binding-symbol index))))
         (target-declaration-specifier
           `(type ,(type-of (storage (target kernel))) target))
         (binding-declaration-specifiers
           (iterate (for binding in-vector (sources kernel)
                         with-index index downto 0)
                    (collect
                        `(type ,(type-of (storage binding))
                               ,(binding-symbol index))
                      at beginning))))
    (apply
     (compile-form
      `(lambda (target ,@binding-symbols)
         (declare
          ,target-declaration-specifier
          ,@binding-declaration-specifiers)
         (%for ,(ranges
                 (funcall
                  (inverse (zero-based-transformation (target kernel)))
                  (index-space kernel)))
               ,(recipe kernel))))
      (storage (target kernel))
      (map 'list #'storage (sources kernel)))))

(defparameter *compile-cache* (make-hash-table :test #'equalp))

(defun compile-form (form)
  (with-hash-table-memoization (form :multiple-values nil)
      *compile-cache*
    (print "Cache miss!")
    (print form)
    (compile nil form)))
