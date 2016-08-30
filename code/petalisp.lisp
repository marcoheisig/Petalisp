;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class operator ()
  (name domain-type codomain-type cycles loads stores))

(define-class structured-operand () (element-type))

(defmacro define-node (name lambda-list slots)
  `(progn
     (define-class ,name (structured-operand) ,slots)
     (defgeneric ,name ,lambda-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; the building blocks of Petalisp

(define-node source (object-or-symbol &key &allow-other-keys) ())

(define-node application (operator object &rest more-objects) (operator objects))

(define-node reduction (operator object) (operator object))

(define-node repetition (object space) (object))

(define-node fusion (object &rest more-objects) (objects))

(define-node reference (object space &optional transformation) (object))

(defgeneric compute (&rest objects))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; operations on index spaces

(define-class index-space (source) ())

(defgeneric index-space (object))

(defgeneric broadcast (space-1 space-2))

(defgeneric intersection (space-1 space-2))

(defgeneric difference (space-1 space-2))

(defgeneric subspace-p (space-1 space-2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; miscellaneous petalisp functions

(defgeneric dimension (object))

(defgeneric size (object))

(defgeneric equalp (object-1 object-2))

(defgeneric compose (object-1 object-2))

(defgeneric invert (object))

(defgeneric transform (object transformation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; argument checking

(defmethod application :before ((operator operator)
                                (object structured-operand)
                                &rest more-objects)
  (assert (= (dimension operator) (1+ (length more-objects))))
  (assert (identical (list* object more-objects)
                     :test #'equalp :key #'index-space)))

(defmethod reduction :before ((operator operator)
                              (object structured-operand))
  (assert (< 0 (dimension object))))

(defmethod repetition :before (object space)
  (assert
   (or
    (< (dimension object) (dimension space))
    (subspace-p (index-space object) space))))

(defmethod fusion :before ((object structured-operand) &rest more-objects)
  (assert (identical (list* object more-objects)
                     :test #'= :key #'dimension)))

(defmethod intersection :before ((space-1 index-space) (space-2 index-space))
  (assert (= (dimension space-1) (dimension space-2))))

(defmethod difference :before ((space-1 index-space) (space-2 index-space))
  (assert (= (dimension space-1) (dimension space-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; default behavior

(defmethod equalp ((object-1 t) (object-2 t))
  (cl:equalp object-1 object-2))

(defmethod compose ((g function) (f function))
  (alexandria:compose g f))

(defmethod index-space ((object index-space)) object)

(defmethod subspace-p (space-1 space-2)
  (equalp space-1 (intersection space-1 space-2)))

(defmethod source ((object structured-operand) &key &allow-other-keys)
  object)

(defmethod intersection ((object-1 structured-operand)
                         (object-2 structured-operand))
  (assert (not (and (index-space-p object-1)
                    (index-space-p object-2))))
  (intersection (index-space object-1) (index-space object-2)))

(defmethod difference ((object-1 structured-operand)
                       (object-2 structured-operand))
  (assert (not (and (index-space-p object-1)
                    (index-space-p object-2))))
  (difference (index-space object-1) (index-space object-2)))
