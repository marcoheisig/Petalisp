;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

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

(defmethod evaluate ((node strided-array-application))
  (let ((args (mapcar (composition #'data #'evaluate) (predecessors node)))
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
