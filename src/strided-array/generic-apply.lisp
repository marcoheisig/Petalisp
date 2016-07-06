;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-application (strided-array application) ())

(defmethod generic-apply ((operator function) (object strided-array)
                          &rest more-objects)
  (let ((objects (list* object more-objects)))
    (let ((element-type
            (apply #'result-type operator
                   (mapcar #'value-type objects)))
          (index-space
            (generic-index-space (first objects))))
      (make-instance
       'strided-array-application
       :operator operator
       :objects objects
       :index-space index-space
       :element-type element-type))))
