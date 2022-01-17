;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.xmas-backend)

(defstruct memory-pool
  (ntype-allocator-vector
   (let* ((size (length petalisp.type-inference:*ntypes*))
          (vector (make-array size)))
     (loop for index below size
           for ntype across petalisp.type-inference:*ntypes*
           do (setf (svref vector index)
                    (make-hash-table :test #'equal)))
     vector)
   :type simple-vector))

(defmacro memory-pool-weak-pointer-list (memory-pool ntype dimensions)
  `(gethash ,dimensions
            (svref (memory-pool-ntype-allocator-vector ,memory-pool)
                   (petalisp.type-inference:ntype-id ,ntype))
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
          (make-array dimensions :element-type (petalisp.type-inference:type-specifier ntype))
          (values array)))))

(defun memory-pool-free (memory-pool array)
  (let ((ntype (petalisp.type-inference:array-element-ntype array))
        (dimensions (array-dimensions array)))
    (symbol-macrolet
        ((weak-pointers (memory-pool-weak-pointer-list memory-pool ntype dimensions)))
      (setf weak-pointers
            (list* (trivial-garbage:make-weak-pointer array)
                   (delete-if-not #'trivial-garbage:weak-pointer-value weak-pointers))))
    (values)))
