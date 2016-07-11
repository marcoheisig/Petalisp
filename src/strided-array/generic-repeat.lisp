;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-repetition (strided-array repetition) ())

(defmethod generic-repeat ((object strided-array)
                           (space strided-array))
  (generic-repeat object (generic-index-space space)))

(defmethod generic-repeat ((object strided-array)
                           (space strided-array-index-space))
  (unless (every #'zerop (mapcar #'rem (ranges object) (ranges space)))
    (error "Unable to repeat ~s to ~s." object space))
  (make-instance
   'strided-array-repetition
   :object object
   :ranges (ranges space)))
