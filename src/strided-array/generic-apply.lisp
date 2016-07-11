;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-application (strided-array application) ())

(defmethod generic-apply ((operator total-function) (object strided-array)
                          &rest more-objects)
  (let ((objects (list* object more-objects)))
    (let ((element-type
            (apply #'result-type operator
                   (mapcar #'codomain-type objects)))
          (ranges (ranges (first objects))))
      (make-instance
       'strided-array-application
       :operator operator
       :objects objects
       :ranges ranges
       :element-type element-type))))
