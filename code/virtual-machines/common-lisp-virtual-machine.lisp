;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class common-lisp-virtual-machine (virtual-machine)
  ((compile-cache :initform (make-hash-table :test #'eq))
   (memory-pool :initform (make-hash-table :test #'equalp))))

(defmethod vm/bind-memory
    ((virtual-machine common-lisp-virtual-machine)
     (immediate strided-array-immediate))
  (let ((array-dimensions
          (map 'list #'size (ranges (index-space immediate))))
        (element-type (element-type immediate)))
    (setf (storage immediate)
          (or
           (pop (gethash (cons element-type array-dimensions)
                         (memory-pool virtual-machine)))
           (make-array array-dimensions :element-type element-type)))))

(defmethod vm/free-memory
    ((virtual-machine common-lisp-virtual-machine)
     (immediate strided-array-immediate))
  (let ((array-dimensions
          (map 'list #'size (ranges (index-space immediate))))
        (element-type (element-type immediate)))
    (push (storage immediate)
          (gethash (cons element-type array-dimensions)
                   (memory-pool virtual-machine)))))

(defun translate-recipe-to-lambda (recipe)
  (with-ustruct-accessors (%recipe) recipe
    (iterate (for range-info in-ulist range-info)
             (collect nil))
    `(lambda () ())))

(defmethod vm/compile
    ((virtual-machine common-lisp-virtual-machine)
     (kernel kernel))
  (let ((recipe (recipe kernel)))
    (with-hash-table-memoization (recipe :multiple-values nil)
        (compile-cache virtual-machine)
      (let ((code (translate-recipe-to-lambda recipe)))
        (print "Cache miss!")
        (print code)
        (finish-output)
        (compile nil code)))))
