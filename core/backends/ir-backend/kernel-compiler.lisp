;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir-backend)

;;; Return a thunk that, when called, executes the kernel.
(defgeneric compile-kernel (kernel))

;;; Return a function that takes an element of the iteration space, i.e., a
;;; list of integers, and that executes this particular instruction.
(defgeneric compile-instruction (instruction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod compile-kernel :around ((kernel petalisp-ir:kernel))
  (let ((*memory-location-table* (make-hash-table :test #'eq)))
    (call-next-method)))

(defmethod compile-kernel ((simple-kernel petalisp-ir:simple-kernel))
  (let ((indices
          (set-elements (petalisp-ir:iteration-space simple-kernel)))
        (body-thunks
          (mapcar #'compile-instruction (petalisp-ir:body simple-kernel))))
    (lambda ()
      (loop for index in indices do
        (loop for body-thunk in body-thunks do
          (funcall body-thunk index))))))

(defmethod compile-kernel ((reduction-kernel petalisp-ir:reduction-kernel))
  (let* ((k (length
             (petalisp-ir:reduction-stores reduction-kernel)))
         (reduction-range
           (petalisp-ir:reduction-range reduction-kernel))
         (inner-indices
           (set-elements
            (petalisp-ir:iteration-space reduction-kernel)))
         (body-thunks
           (mapcar #'compile-instruction (petalisp-ir:body reduction-kernel)))
         (load-thunks
           (loop for i below k
                 collect
                 (make-load-thunk (petalisp-ir:reduction-value-symbol i))))
         (store-thunks
           (mapcar #'make-store-thunk (petalisp-ir:reduction-stores reduction-kernel))))
    (lambda ()
      (loop for inner-index in inner-indices do
        (labels ((divide-and-conquer (range)
                   (if (unary-range-p range)
                       (let ((index (cons (range-start range) inner-index)))
                         (loop for body-thunk in body-thunks do
                           (funcall body-thunk index))
                         (values-list
                          (loop for load-thunk in load-thunks
                                collect (funcall load-thunk index))))
                       (multiple-value-bind (left right)
                           (split-range range)
                         (values-list
                          (subseq
                           (multiple-value-list
                            (multiple-value-call (operator reduction-kernel)
                              (divide-and-conquer left)
                              (divide-and-conquer right)))
                           0 k))))))
          (loop for value in (multiple-value-list (divide-and-conquer reduction-range))
                for store-thunk in store-thunks do
                  (funcall store-thunk inner-index value)))))))

(defmethod compile-instruction ((instruction petalisp-ir:instruction))
  (let ((load-thunks
          (mapcar #'make-load-thunk (petalisp-ir:loads instruction)))
        (store-thunks
          (mapcar #'make-store-thunk (petalisp-ir:stores instruction)))
        (op (petalisp-ir:operator instruction)))
    (lambda (index)
      (let ((args (loop for load-thunk in load-thunks
                        collect (funcall load-thunk index))))
        (loop for value in (multiple-value-list (apply op args))
              for store-thunk in store-thunks do
                (funcall store-thunk index value))))))
