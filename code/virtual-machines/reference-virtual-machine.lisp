;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class reference-virtual-machine (virtual-machine)
  ((evaluated-nodes :type hash-table :initform (make-hash-table :test #'eq))))

(defmethod vm/schedule ((vm reference-virtual-machine) targets recipes)
  (iterate
    (for target in-sequence targets)
    (for recipe in-sequence recipes)
    (setf (storage target)
          (storage (reference-vm/evaluate-node vm recipe))))
  (clrhash (evaluated-nodes vm))
  (complete (make-request)))

(defgeneric reference-vm/evaluate-node (virtual-machine node)
  (:documentation
   "Return a list whose elements are of the form
    (value index-0 ... index-N)")
  (:method :around ((vm reference-virtual-machine) (node data-structure))
    (with-hash-table-memoization (node)
        (evaluated-nodes vm)
      (call-next-method))))

(defun reference-vm/normalize-result (result)
  (let ((dimension (length (car (first result)))))
    (loop for (indices . value) in result do
      (assert (= (length indices) dimension))))
  (flet ((list-lessp (list-1 list-2)
           (loop for element-1 in list-1
                 for element-2 in list-2
                 do (unless (= element-1 element-2)
                      (return (< element-1 element-2))))))
    (sort result #'list-lessp :key #'first)))

(defmethod reference-vm/evaluate-node
    ((vm reference-virtual-machine) (node immediate))
  (let ((list-of-indices
          (apply #'map-product #'list
                 (map 'list #'depetalispify
                      (ranges (index-space node))))))
    (reference-vm/normalize-result
     (loop for indices in list-of-indices
           collect
           (cons
            indices
            (apply #'aref (storage node)
                   (funcall (to-storage node) indices)))))))

(defmethod reference-vm/evaluate-node
    ((vm reference-virtual-machine) (node application))
  (let ((operator (operator node))
        (inputs (mapcar #'reference-vm/evaluate-node (inputs node))))
    (apply #'mapcar
           (lambda (&rest inputs)
             (apply #'mapcar inputs)
             (assert (identical inputs :test #'equal :key #'car))
             (cons
              (car (first inputs))
              (apply operator (mapcar #'cdr inputs))))
           inputs)))

(defmethod reference-vm/evaluate-node
    ((vm reference-virtual-machine) (node reduction))
  )

(defmethod reference-vm/evaluate-node
    ((vm reference-virtual-machine) (node fusion))
  )

(defmethod reference-vm/evaluate-node
    ((vm reference-virtual-machine) (node reference))
  )
