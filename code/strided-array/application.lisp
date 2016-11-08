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
