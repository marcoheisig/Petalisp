;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp core functions

(in-package :petalisp)

(defgeneric generic-apply (operator object &rest more-objects))

(defgeneric generic-reduce (operator object))

(defgeneric generic-broadcast (object-1 object-2))

(defgeneric generic-dimension (object))

(defgeneric generic-equalp (object-1 object-2))

(defgeneric generic-repeat (object space))

(defgeneric generic-fuse (object &rest more-objects))

(defgeneric generic-index-space (object))

(defgeneric generic-input (object-or-symbol &rest arguments))

(defgeneric generic-intersect (object-1 object-2))

(defgeneric generic-invert (transformation))

(defgeneric generic-select (object space))

(defgeneric generic-size (object))

(defgeneric generic-transform (object transformation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; documentation of the core functions

(setf (documentation #'generic-apply 'function)
      "Apply OPERATOR element-wise to the given objects. The number of
      objects must match the arity of OPERATOR, and all objects must have
      the same shape.")

(setf (documentation #'generic-reduce 'function)
      "Returns an instance of MAPPING that is obtained by reducing the
      elements of the trailing dimension of OBJECT by successive
      applications of the binary operator OPERATOR. Signals an error if
      OBJECT has dimension zero.")

(setf (documentation #'generic-broadcast 'function)
      "Returns an instance of INDEX-SPACE such that both OBJECT-1 and
      OBJECT-2 can be extended to it via GENERIC-REPEAT. Signals an error
      if no such index space can be found.")

(setf (documentation #'generic-dimension 'function)
      "Returns the dimension of OBJECT, that is how often one successively
      apply GENERIC-REDUCE to it.")

(setf (documentation #'generic-equalp 'function)
      "Returns true when OBJECT-1 and OBJECT-2 are equal in the sense of
      Petalisp.")

(setf (documentation #'generic-repeat 'function)
      "Returns an instance of MAPPING with the same shape as SPACE and with
      the values of OBJECT as if multiple copies of OBJECT were translated
      via GENERIC-TRANSFORM so that they could be combined via
      GENERIC-FUSE. Signals an error is no suitable repetition is found.")

(setf (documentation #'generic-fuse 'function)
      "Returns an instance of MAPPING that corresponds to the union of all
      key-value pairs of all given objects. Signals an error if multiple
      objects contain the same key, or if the union can not be suitably
      represented by any Petalisp datastructure.")

(setf (documentation #'generic-index-space 'function)
      "Returns an instance of INPUT with the same shape as OBJECT, where
      each key maps to itself.")

(setf (documentation #'generic-input 'function)
      "Returns an instance of INPUT by dispatching on OBJECT-OR-SYMBOL,
      with further customization according to ARGUMENTS.")

(setf (documentation #'generic-intersect 'function)
      "Returns an instance of INDEX-SPACE containing all keys that are both
      in OBJECT-1 and OBJECT-2. Returns false if OBJECT-1 and OBJECT-2 have
      no common keys.")

(setf (documentation #'generic-invert 'function)
      "Returns the inverse function of TRANSFORMATION, such that the
      composition of TRANSFORMATION and (GENERIC-INVERT TRANSFORMATION)
      does nothing.")

(setf (documentation #'generic-select 'function)
      "Returns an instance of MAPPING that contains all elements of OBJECT
      that are denoted by SPACE. Signals an error if SPACE does not denote
      a proper subspace of OBJECT.")

(setf (documentation #'generic-size 'function)
      "Returns the number of key-value relations in OBJECT.")

(setf (documentation #'generic-transform 'function)
      "Returns an instance of MAPPING that is obtained by applying
      TRANSFORMATION to the keys of OBJECT.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp root classes

(defclass petalisp-function () ())

(defclass application (petalisp-function)
  ((%operator :initarg :operator :reader operator)
   (%objects :initarg :objects :reader objects)))

(defclass reduction (petalisp-function)
  ((%operator :initarg :operator :reader operator)
   (%object :initarg :object :reader object)))

(defclass repetition (petalisp-function)
  ((%object :initarg :object :reader object)))

(defclass fusion (petalisp-function)
  ((%objects :initarg :objects :reader objects)))

(defclass selection (petalisp-function)
  ((%object :initarg :object :reader object)))

(defclass transformation (petalisp-function)
  ((%object :initarg :object :reader object)
   (%transformation :initarg :transformation :reader transformation)))

(defclass mapping ()
  ((%key-type :initarg :key-type :reader key-type)
   (%value-type :initarg :value-type :reader value-type)))

(defclass operator ()
  ((%arity :initarg :arity :reader arity)))

(defclass input (mapping) ())

(defclass index-space (input) ()
  (:documentation
   "A Petalisp object that where each value equals its key."))

(defclass constant (input)
  ((%object :initarg :object :reader object))
  (:documentation
   "A Petalisp object that is constructed from a given lisp constant."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; basic error checking for the core functions

;;; TODO typechecking of operators and transformations

(defmethod generic-apply :before ((operator operator) (object mapping)
                                  &rest more-objects)
  (assert (= (arity operator) (1+ (length more-objects))))
  (assert (not (find (generic-index-space object)
                     (mapcar #'generic-index-space more-objects)
                     :test (complement #'generic-equalp)))))

(defmethod generic-reduce :before ((operator operator) (object mapping))
  (assert (< 1 (generic-dimension object))))

(defmethod generic-select ((object mapping) (space mapping))
  (assert (generic-equalp (generic-index-space space)
                          (generic-intersect object space))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; embedding Petalisp in Common Lisp

(defmethod generic-input ((object mapping) &rest arguments)
  (declare (ignore arguments))
  object)

(defun index-space (object)
  (generic-input 'index-space object))

(defun α (operator object &rest more-objects)
  (let* ((objects
           (mapcar #'generic-input (list* object more-objects)))
         (index-space
           (reduce #'generic-broadcast objects))
         (objects
           (mapcar
            (lambda (object)
              (generic-repeat object index-space))
            objects))
         (operator (find-operator operator)))
    (apply #'generic-apply operator objects)))

(defun β (operator object)
  (generic-reduce operator object))

(defun repeat (object space)
  (generic-repeat object space))

(defun select (object indices)
  (generic-repeat object indices))

(defun transform (object transformation)
  (generic-transform object transformation))

(defun fuse (object &rest more-objects)
  (apply #'generic-fuse object more-objects))

(defun size (object)
  (generic-size object))

(defun dimension (object)
  (generic-dimension object))

(defun broadcast (object &rest more-objects)
  (reduce #'generic-broadcast more-objects
          :initial-value object))
