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
  (when (or (> (size object) *constant-fold-threshold*)
            (not (every #'strided-array-constant? more-objects)))
    (return-from application (call-next-method)))
  ;; now the actual constant folding
  (call-next-method)) ; TODO
