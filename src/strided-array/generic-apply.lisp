;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-application (strided-array application) ())

(defmethod generic-α ((operator function) (argument strided-array)
                      &rest more-arguments)
  (let* ((arguments
           (mapcar #'make-lisp-input arguments))
         (index-space
           (apply #'index-space-broadcast
                  (mapcar #'index-space arguments)))
         (arguments
           (mapcar
            (lambda (argument)
              (replicate argument index-space))
            arguments))
         (element-type
           (apply #'result-type operator
                  (mapcar #'element-type arguments))))
    (make-instance
     'α
     :operator operator
     :arguments arguments
     :index-space index-space
     :element-type element-type)))
