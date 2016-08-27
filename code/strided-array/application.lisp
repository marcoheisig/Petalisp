;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-application (strided-array application) ())

(defmethod application ((operator operator) (object strided-array)
                        &rest more-objects)
  (let ((objects (list* object more-objects)))
    (let ((ranges (ranges (first objects))))
      (make-instance
       'strided-array-application
       :operator operator
       :objects objects
       :ranges ranges))))
