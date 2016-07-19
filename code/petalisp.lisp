;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class total-function () (codomain-type domain-type))

(define-class node (total-function) ())

(defmacro define-node (name lambda-list &optional (slots () slots-p))
  (let ((slots (if slots-p slots
                   (remove-if
                    (lambda (x) (member x lambda-list-keywords))
                    lambda-list))))
    `(progn
       (define-class ,name (node) ,slots)
       (defgeneric ,name ,lambda-list))))

(define-node application (operator object &rest more-objects) (operator objects))

(define-node reduction (operator object))

(define-node repetition (object space) (object))

(define-node fusion (object &rest more-objects) (objects))

(define-node selection (object space) (object))

(define-node transformation (object &key &allow-other-keys) (object))

(define-node source (object-or-symbol &rest arguments) ())

(define-node target (object target-or-symbol &rest arguments) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; operations on index spaces

(define-class index-space (source) ())

(defgeneric index-space (object))

(defgeneric broadcast (space-1 space-2))

(defgeneric intersection (space-1 space-2))

(defgeneric difference (space-1 space-2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; miscellaneous petalisp functions

(defgeneric dimension (object))

(defgeneric size (object))

(defgeneric equalp (object-1 object-2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; default behavior

(defmethod application :before ((operator total-function)
                                (object total-function)
                                &rest more-objects)
  (assert (= (dimension operator) (1+ (length more-objects))))
  (assert (not (find (index-space object)
                     (mapcar #'index-space more-objects)
                     :test (complement #'equalp)))))

(defmethod fusion ((object total-function) &rest more-objects)
  (assert (apply #'= (mapcar #'dimension (list* object more-objects)))))

(defmethod reduction :before ((operator total-function)
                              (object total-function))
  (assert (< 1 (dimension object))))

(defmethod selection ((object total-function)
                      (space total-function))
  (assert (equalp (index-space space)
                  (intersection object space))))

(defmethod source ((object source) &rest arguments)
  (assert (null arguments))
  object)

(defmethod equalp ((object-1 t) (object-2 t))
  (cl:equalp object-1 object-2))

(defmethod index-space ((object index-space)) object)
