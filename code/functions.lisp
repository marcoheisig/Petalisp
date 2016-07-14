;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defgeneric make-application (operator object &rest more-objects))

(defgeneric make-reduction (operator object))

(defgeneric make-repetition (object space))

(defgeneric make-fusion (object &rest more-objects))

(defgeneric make-selection (object space))

(defgeneric make-source (object-or-symbol &rest arguments))

(defgeneric make-target (object target-or-symbol &rest arguments))

(defgeneric make-transformation (object &key &allow-other-keys))

(defgeneric dimension (object))

(defgeneric size (object))

(defgeneric equal? (object-1 object-2))

(defgeneric index-space (object))

(defgeneric broadcast (object-1 object-2))

(defgeneric intersection (object-1 object-2))

(defgeneric value (symbol space dimension))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; default behavior

(defmethod make-application :before ((operator total-function)
                                     (object total-function)
                                     &rest more-objects)
  (assert (= (dimension operator) (1+ (length more-objects))))
  (assert (not (find (index-space object)
                     (mapcar #'index-space more-objects)
                     :test (complement #'equal?)))))

(defmethod make-reduction :before ((operator total-function)
                                   (object total-function))
  (assert (< 1 (dimension object))))

(defmethod make-selection ((object total-function)
                           (space total-function))
  (assert (equal? (index-space space)
                  (intersection object space))))

(defmethod make-source ((object source) &rest arguments)
  (declare (ignore arguments))
  object)

(defmethod make-fusion ((object total-function) &rest more-objects)
  (assert (apply #'= (mapcar #'dimension (list* object more-objects)))))
