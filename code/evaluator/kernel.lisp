;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class kernel ()
  ((recipe :type data-structure)
   (cost :type non-negative-integer)
   (number-of-dependencies :type non-negative-integer)
   (users :type list))
  (:documentation
   "The kernel is a fundamental building block of Petalisp evaluation. Its
   RECIPE is a graph of data structures, whose nodes are the input of at
   most one other data structure."))

(defun make-kernel (recipe)
  (make-instance 'kernel
    :recipe recipe
    :cost (size recipe)
    :number-of-dependencies 0
    :users ()))

(defun kernelize (graph-roots)
  "Return a list of kernels whose evaluation is equivalent to the
evaluation of the given GRAPH-ROOTS."
  (let ((use-table (successor-table graph-roots #'inputs))
        (kernel-table (make-hash-table :test #'eq)))
    (labels ((start-new-kernel (node)
               "Use node as the recipe of a new kernel. Return an immediate
               value holding the (not yet computed) result of NODE."
               (let ((kernel (gethash node kernel-table)))
                 (cond
                   ((immediate? node) node) ; no need to kernelize immediates
                   (kernel (kernel-target kernel)) ; reuse already kernelized nodes
                   (t (make-kernel (kernelize-recipe node))))))
             (kernelize-recipe (node)
               (cond ((immediate? node) node)
                     (t)))))))
