;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defgeneric evaluate (object))

(defmethod evaluate ((strided-array-immediate strided-array-immediate))
  (unless (storage strided-array-immediate) ; only evaluate once
    ;; evaluate all dependencies
    (map nil #'evaluate (dependencies strided-array-immediate))
    ;; allocate memory
    (bind-memory strided-array-immediate)
    ;; compute all kernels
    (map nil #'evaluate (kernels strided-array-immediate))
    ;; release resources
    (iterate
      (for dependency in-vector (dependencies strided-array-immediate))
      ;(fvector-remove strided-array-immediate (users dependency))
      (when (emptyp (users dependency))
        (free-memory dependency)))
    strided-array-immediate))

(defmethod evaluate ((kernel kernel))
  (let* ((source-symbols
           (iterate (for index below (length (sources kernel)))
                    (collect (%source index))))
         (target-declaration-specifier
           `(type ,(type-of (storage (target kernel))) target))
         (source-declaration-specifiers
           (iterate (for source in-vector (sources kernel)
                         with-index index downto 0)
                    (collect
                        `(type ,(type-of (storage source))
                               ,(%source index))
                      at beginning))))
    (apply
     (compile-form
      `(lambda (target ,@source-symbols)
         (declare
          ,target-declaration-specifier
          ,@source-declaration-specifiers)
         (%for ,(ranges
                 (funcall
                  (inverse (zero-based-transformation (target kernel)))
                  (index-space kernel)))
               ,(recipe kernel))))
     (storage (target kernel))
     (map 'list #'storage (sources kernel)))))
