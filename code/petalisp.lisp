;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class operator () (domain-type codomain-type))

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

(define-node reference (object &rest subspaces-and-transformations)
  (object source-space target-space))

(defgeneric compute (&rest objects))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; operations on index spaces

(define-class index-space (source) ())

(defgeneric index-space (object))

(defgeneric broadcast (space-1 space-2))

(defgeneric intersection (space-1 space-2))

(defgeneric difference (space-1 space-2))

(defgeneric transform (space &key translation scaling permutation))

(defgeneric inverse-transform (space &key translation scaling permutation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; miscellaneous petalisp functions

(defgeneric dimension (object))

(defgeneric size (object))

(defgeneric equalp (object-1 object-2))

(defgeneric compose (object-1 object-2))

(defgeneric invert (object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; default behavior

(defmethod application :before ((operator operator)
                                (object structured-operand)
                                &rest more-objects)
  (assert (= (dimension operator) (1+ (length more-objects))))
  (assert (identical (list* object more-objects)
                     :test #'equalp :key #'index-space)))

(defmethod fusion ((object structured-operand) &rest more-objects)
  (assert (identical (list* object more-objects)
                     :test #'= :key #'dimension)))

(defmethod reduction :before ((operator operator)
                              (object structured-operand))
  (assert (< 0 (dimension object))))

(defmethod reference :before (object &rest subspaces-and-transformations)
  ;; 1. compute target space
  ;; 2. compose all transformations
  ;; 3. apply the inverse to the target-space -> source-space
  (let ((target-space (index-space object)))
    (dolist (x subspaces-and-transformations)
      (etypecase x
        (transformation
         (zapf target-space (transform % x)))
        (index-space
         (assert (subspace-p x target-space))
         (setf target-space x))))
    target-space))

(defmethod equalp ((object-1 t) (object-2 t))
  (cl:equalp object-1 object-2))

(defmethod compose ((g function) (f function))
  (alexandria:compose g f))

(defmethod index-space ((object index-space)) object)

(defmethod reference ((object t) &key source-space target-space
                                   translation scaling permutation)
  (let ((class
          (cond ((and translation scaling permutation) 'tsp-reference)
                ((and translation scaling) 'ts-reference)
                ((and translation permutation) 'tp-reference)
                ((and scaling permutation) 'sp-reference)
                (translation 'translation)
                (scaling 'scaling)
                (permutation 'permutation)))
        (source-space
          (or source-space
              (and target-space
                   (apply #'inverse-transform target-space args))
              (index-space object)))
        (target-space
          (or target-space
              (apply #'transform source-space args))))))
