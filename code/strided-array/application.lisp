;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-application (strided-array application) ())

(defmethod application ((operator function) (object strided-array)
                        &rest more-objects)
  (let ((objects (cons object more-objects)))
    (make-instance
     'strided-array-application
     :operator operator
     :element-type (apply #'result-type operator
                          (mapcar #'element-type objects))
     :predecessors objects
     :ranges (ranges object))))

(defmethod application ((operator function) (object strided-array-constant)
                        &rest more-objects)
  (if (and (<= (size object) *constant-fold-threshold*)
           (every #'strided-array-constant? more-objects))
      (evaluate-node (call-next-method)) ; constant folding
      (call-next-method)))

(defmethod evaluate-node ((node strided-array-application))
  (let ((args (mapcar #'evaluate-node (predecessors node)))
        (op (operator node)))
    (make-instance
     'strided-array-constant
     :data array
     :affine-coefficients TODO
     :element-type (element-type node)
     :ranges (ranges node))))
