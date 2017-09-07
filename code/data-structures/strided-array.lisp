;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array (data-structure)
  (index-space))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; subclasses of the class STRIDED-ARRAY and corresponding constructors

;; note: the fact that APPLICATION appears before STRIDED-ARRAY in the
;; following list of superclasses is meaningful. We want methods
;; specializing on APPLICATION (graph optimizations) to be more specific
;; than methods specializing on STRIDED-ARRAY. The same holds for all the
;; other subclasses of STRIDED-ARRAY.
(define-class strided-array-application (application strided-array) ())

(defmethod application ((operator function) (object strided-array)
                        &rest more-objects)
  (let ((objects (cons object more-objects)))
    (make-instance
     'strided-array-application
     :operator operator
     :element-type (apply #'result-type operator
                          (mapcar #'element-type objects))
     :predecessors objects
     :index-space (index-space object))))

(define-class strided-array-elaboration (elaboration strided-array)
  ((data :type array)))

(defmethod petalispify ((array array))
  (make-instance
   'strided-array-elaboration
   :element-type (array-element-type array)
   :data array
   :index-space (make-strided-array-index-space array)))

(defmethod depetalispify ((instance strided-array-elaboration))
  (data instance))

(define-class strided-array-fusion (fusion strided-array) ())

(defmethod fusion ((object strided-array) &rest more-objects)
  (let ((objects (cons object more-objects)))
    (make-instance
     'strided-array-fusion
     :element-type (element-type object)
     :predecessors objects
     :index-space (apply #'fusion (mapcar #'index-space objects)))))

(define-class strided-array-reduction (reduction strided-array) ())

(defmethod reduction ((operator function) (object strided-array))
  (let ((ranges (ranges (index-space object))))
    (make-instance
     'strided-array-reduction
     :operator operator
     :element-type (element-type object)
     :predecessors (list object)
     :index-space (make-strided-array-index-space
                   (subseq ranges 0 (1- (length ranges)))))))

(define-class strided-array-reference (reference strided-array) ())

(defmethod reference ((object strided-array)
                      (space strided-array-index-space)
                      (transformation transformation))
  (make-instance
   'strided-array-reference
   :predecessors (list object)
   :index-space space
   :element-type (element-type object)
   :transformation transformation))
