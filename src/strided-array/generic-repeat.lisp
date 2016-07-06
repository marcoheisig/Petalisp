;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-repetition (strided-array repetition) ())

(defmethod generic-repeat ((object strided-array)
                           (space strided-array-index-space))
  "Returns a Petalisp object with the shape RESULT-SPACE that is obtained
by replicating the content of object enough times. An error is signaled if
there is a dimension where the number of elements in RESULT-SPACE is not
divisible by the number of elements in OBJECT."
  (unless (every #'zerop (mapcar #'rem (ranges object) (ranges space)))
    (error "Unable to repeat ~s to ~s." object space))
  (make-instance
   'strided-array-repetition
   :object object
   :ranges (ranges space)
   :element-type (value-type object)))
