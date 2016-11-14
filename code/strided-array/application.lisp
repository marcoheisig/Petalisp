;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-application (strided-array application) ())

(defmethod application ((operator function) (object strided-array)
                        &rest more-objects)
  (make-instance
   'strided-array-application
   :operator operator
   :predecessors (list* object more-objects)
   :ranges (ranges object)))

(defmethod application :around ((operator function) (object strided-array-constant)
                                &rest more-objects)
  (flet ((foldable (object)
           (and (< (size object) 42) ; constant fold only small arrays
                (strided-array-constant? object))))
    (let ((objects (cons object more-objects)))
      (declare (dynamic-extent objects))
      (cond
        ((not (every #'foldable objects))
         (call-next-method))
        (t
         (assert (identical objects :test #'equal? :key #'index-space))
         (let ((array (apply #'array-map operator
                             (mapcar #'data objects))))
           (make-instance
            'strided-array-constant
            :element-type (element-type array)
            :data array
            :ranges (ranges object))))))))
