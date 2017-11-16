;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; The purpose of the reference virtual machine is to compute reference
;;; solutions for automated testing. It is totally acceptable if this
;;; implementation is slow or eagerly consing, as long as it is obviously
;;; correct.
;;;
;;; Internally, all evaluated arrays are represented as a list of conses of
;;; the form (indices . value), where indices is a list of integers.

(define-class reference-virtual-machine (virtual-machine)
  ((evaluated-nodes :type hash-table :initform (make-hash-table :test #'eq))))

(defmethod vm/schedule ((vm reference-virtual-machine) targets recipes)
  (iterate
    (for target in-sequence targets)
    (for recipe in-sequence recipes)
    (setf (storage target)
          (make-array
           (map 'list #'size (ranges (index-space target)))
           :element-type (element-type target)))
    (reference-vm/copy
     (vm/evaluate vm recipe)
     target))
  (clrhash (evaluated-nodes vm))
  (complete (make-request)))

(defun reference-vm/normalize (result)
  "Assert that RESULT satisfies the reference virtual machine array
   representation and ensure the canonical ordering of indices."
  (let ((dimension (length (car (first result)))))
    (loop for (indices . value) in result do
      (assert (= (length indices) dimension))))
  (flet ((list-lessp (list-1 list-2)
           (loop for element-1 in list-1
                 for element-2 in list-2
                 do (unless (= element-1 element-2)
                      (return (< element-1 element-2))))))
    (sort result #'list-lessp :key #'first)))

(defgeneric reference-vm/represent (immediate)
  (:method ((strided-array-index-space strided-array-index-space))
    (if (= 0 (dimension strided-array-index-space))
        (list ())
        (apply #'map-product #'list
               (map 'list #'reference-vm/represent
                    (ranges strided-array-index-space)))))
  (:method ((range range))
    (loop for i
          from (range-start range)
            by (range-step range)
            to (range-end range)
          collect i)))

(defgeneric reference-vm/copy (from to)
  (:method ((internal-representation list)
            (strided-array-immediate strided-array-immediate))
    (loop for (indices . value) in internal-representation do
      (setf (apply #'aref (storage strided-array-immediate)
                   (funcall (to-storage strided-array-immediate) indices))
            value))))

(defmethod vm/evaluate :around
    ((vm reference-virtual-machine) (node data-structure))
  (with-hash-table-memoization (node)
      (evaluated-nodes vm)
    (reference-vm/normalize
     (call-next-method))))

(defmethod vm/evaluate
    ((vm reference-virtual-machine) (node immediate))
  (let ((list-of-indices (reference-vm/represent (index-space node))))
    (loop for indices in list-of-indices
          collect
          (cons
           indices
           (apply #'aref (storage node)
                  (funcall (to-storage node) indices))))))

(defmethod vm/evaluate
    ((vm reference-virtual-machine) (node application))
  (flet ((indices (input) (car input))
         (value (input) (cdr input))
         (evaluate (node)
           (vm/evaluate vm node)))
    (let ((operator (operator node)))
      (apply #'mapcar
             (lambda (&rest inputs)
               (assert (identical inputs :test #'equal :key #'indices))
               (cons
                (indices (first inputs))
                (apply operator (mapcar #'value inputs))))
             (mapcar #'evaluate (inputs node))))))

(defmethod vm/evaluate
    ((vm reference-virtual-machine) (node reduction))
  (let ((input (vm/evaluate vm (input node)))
        (operator (operator node))
        result)
    (loop for (indices . value) in input do
      (let ((new-indices (butlast indices)))
        (if-let ((found (assoc new-indices result :test #'equal)))
          (setf (cdr found)
                (funcall operator (cdr found) value))
          (push (cons new-indices value) result))))
    result))

(defmethod vm/evaluate
    ((vm reference-virtual-machine) (node fusion))
  (flet ((evaluate (node)
           (vm/evaluate vm node)))
    (apply #'append (mapcar #'evaluate (inputs node)))))

(defmethod vm/evaluate
    ((vm reference-virtual-machine) (node reference))
  (let ((list-of-indices (reference-vm/represent (index-space node)))
        (input (vm/evaluate vm (input node))))
    (loop for indices in list-of-indices
          collect
          (flet ((indices (input) (car input))
                 (value (input) (cdr input)))
            (cons indices
                  (value
                   (find (funcall (transformation node) indices)
                         input
                         :test #'equal
                         :key #'indices)))))))
