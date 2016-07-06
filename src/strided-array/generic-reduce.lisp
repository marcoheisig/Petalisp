;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass strided-array-reduction (strided-array reduction) ())

(defmethod generic-reduce ((operator function)
                           (object strided-array))
  (let ((result-space
          (index-space-drop-last-dimension
           (index-space argument)))
        (element-type (element-type argument)))
    (assert (petalisp-subtypep
             (result-type operator element-type element-type)
             element-type))
    (make-instance
     'strided-array-reduction
     :operator operator
     :argument argument
     :index-space result-space
     :element-type element-type)))
