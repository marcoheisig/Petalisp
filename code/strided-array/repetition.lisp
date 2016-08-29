;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-repetition (strided-array repetition) ())

(defmethod repetition ((object strided-array)
                       (space strided-array))
  (repetition object (index-space space)))

(defmethod repetition ((object strided-array)
                       (space strided-array-index-space))
  TODO
  (make-instance
   'strided-array-repetition
   :object object
   :ranges (ranges space)))
