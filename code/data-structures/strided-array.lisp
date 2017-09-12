;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array (data-structure)
  (index-space))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; subclasses of the class STRIDED-ARRAY and corresponding constructors

(define-class strided-array-application (strided-array application) ())

(defmethod application ((operator function) (object strided-array)
                        &rest more-objects)
  (let ((objects (cons object more-objects)))
    (make-instance
     'strided-array-application
     :operator operator
     :element-type (apply #'result-type operator
                          (mapcar #'element-type objects))
     :inputs objects
     :index-space (index-space object))))

(defmethod shallow-copy ((object strided-array-application))
  (make-instance
   'strided-array-application
   :operator (operator object)
   :element-type (element-type object)
   :inputs (inputs object)
   :index-space (index-space object)))

(define-class strided-array-elaboration (strided-array elaboration)
  ((data :type array)))

(defmethod petalispify ((array array))
  (make-instance
   'strided-array-elaboration
   :element-type (array-element-type array)
   :data array
   :index-space (make-strided-array-index-space array)))

(defmethod depetalispify ((instance strided-array-elaboration))
  (data instance))

(define-class strided-array-fusion (strided-array fusion) ())

(defmethod fusion ((object strided-array) &rest more-objects)
  (let ((objects (cons object more-objects)))
    (make-instance
     'strided-array-fusion
     :element-type (element-type object)
     :inputs objects
     :index-space (apply #'fusion (mapcar #'index-space objects)))))

(defmethod shallow-copy ((object strided-array-fusion))
  (make-instance
   'strided-array-fusion
   :element-type (element-type object)
   :inputs (inputs object)
   :index-space (index-space object)))

(define-class strided-array-reduction (strided-array reduction) ())

(defmethod reduction ((operator function) (object strided-array))
  (let ((ranges (ranges (index-space object))))
    (make-instance
     'strided-array-reduction
     :operator operator
     :element-type (element-type object)
     :inputs (list object)
     :index-space (make-strided-array-index-space
                   (subseq ranges 0 (1- (length ranges)))))))

(defmethod shallow-copy ((object strided-array-reduction))
  (make-instance
   'strided-array-reduction
   :operator (operator object)
   :element-type (element-type object)
   :inputs (inputs object)
   :index-space (index-space object)))

(define-class strided-array-reference (strided-array reference) ())

(defmethod reference ((object strided-array)
                      (space strided-array-index-space)
                      (transformation transformation))
  (make-instance
   'strided-array-reference
   :element-type (element-type object)
   :inputs (list object)
   :index-space space
   :transformation transformation))

(defmethod shallow-copy ((object strided-array-reference))
  (make-instance
   'strided-array-reference
   :element-type (element-type object)
   :inputs (inputs object)
   :index-space (index-space object)
   :transformation (transformation object)))
