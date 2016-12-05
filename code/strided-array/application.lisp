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
      (evaluate (call-next-method)) ; constant folding
      (call-next-method)))

(defmethod evaluate ((node strided-array-application))
  (let ((args (mapcar (compose #'data #'evaluate) (predecessors node)))
        (op (operator node))
        (result (make-array
                 (map 'list #'size (ranges node))
                 :element-type (element-type node))))
    (apply #'array-map op result args)
    (make-instance
     'strided-array-constant
     :data result
     :element-type (element-type node)
     :ranges (ranges node))))
