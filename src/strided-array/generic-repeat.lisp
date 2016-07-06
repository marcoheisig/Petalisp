;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-repetition (strided-array repetition) ())

(defmethod generic-repeat ((object strided-array)
                           (space strided-array-index-space))
  "Returns a Petalisp object with the shape RESULT-SPACE that is obtained
by replicating the content of object enough times. An error is signaled if
there is a dimension where the number of elements in RESULT-SPACE is not
divisible by the number of elements in OBJECT."
  (when (generic-equalp result-space (index-space object))
    (return-from generic-repeat object))
  (unless (every #'integerp
                 (mapcar
                  (lambda (src-range dst-range)
                    (/ (range-elements dst-range)
                       (range-elements src-range)))
                  (index-space object)
                  result-space))
    (error 'unable-to-repeat
           :object object
           :index-space result-space))
  (make-instance
   'strided-array-repetition
   :object object
   :index-space result-space
   :element-type (element-type object)))
