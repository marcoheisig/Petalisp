;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.xmas-backend)

(defstruct (memory-pool
            (:predicate memory-pool-p)
            (:constructor make-memory-pool ()))
  (ntype-allocator-vector
   (let* ((size typo:+primitive-ntype-limit+)
          (vector (make-array size)))
     (loop for index below typo:+primitive-ntype-limit+
           do (setf (svref vector index)
                    (make-hash-table :test #'equal)))
     vector)
   :type simple-vector
   :read-only t))

(defmacro memory-pool-weak-pointer-list (memory-pool ntype dimensions)
  `(gethash ,dimensions
            (svref (memory-pool-ntype-allocator-vector ,memory-pool)
                   (typo:ntype-index ,ntype))
            '()))

(defun memory-pool-allocate (memory-pool ntype dimensions)
  (symbol-macrolet
      ((weak-pointers (memory-pool-weak-pointer-list memory-pool ntype dimensions)))
    (let ((list '())
          (array nil))
      (loop for weak-pointer in weak-pointers
            for value = (trivial-garbage:weak-pointer-value weak-pointer)
            when value do
              (if (null array)
                  (setf array value)
                  (push weak-pointer list)))
      (setf weak-pointers list)
      (if (null array)
          (make-array dimensions :element-type (typo:ntype-type-specifier ntype))
          (values array)))))

(defun memory-pool-free (memory-pool array)
  (let ((ntype (typo:array-element-ntype array))
        (dimensions (array-dimensions array)))
    (symbol-macrolet
        ((weak-pointers (memory-pool-weak-pointer-list memory-pool ntype dimensions)))
      (setf weak-pointers
            (list* (trivial-garbage:make-weak-pointer array)
                   (delete-if-not #'trivial-garbage:weak-pointer-value weak-pointers))))
    (values)))
