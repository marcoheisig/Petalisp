;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defgeneric generic-apply (operator object &rest more-objects))

(defgeneric generic-broadcast (object-1 object-2))

(defgeneric generic-dimension (object))

(defgeneric generic-equalp (object-1 object-2))

(defgeneric generic-fuse (object &rest more-objects))

(defgeneric generic-index-space (object))

(defgeneric generic-intersect (object-1 object-2))

(defgeneric generic-invert (transformation))

(defgeneric generic-reduce (operator object))

(defgeneric generic-repeat (object space))

(defgeneric generic-select (object space))

(defgeneric generic-size (object))

(defgeneric generic-source (object-or-symbol &rest arguments))

(defgeneric generic-target (object target-or-symbol &rest arguments))

(defgeneric generic-transform (object &key scaling translation permutation
                               &allow-other-keys))

(defgeneric magic-symbol-value (symbol space dimension))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; documentation

(setf (documentation #'generic-apply 'function)
      "Apply OPERATOR element-wise to the given objects. The number of
      objects must match the arity of OPERATOR, and all objects must have
      the same shape.")

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

(setf (documentation #'generic-fuse 'function)
      "Returns an instance of TOTAL-FUNCTION that corresponds to the union
      of all relations of all given objects. Signals an error if the result
      is not a total function, or if the union can not be suitably
      represented by any Petalisp datastructure.")

(setf (documentation #'generic-index-space 'function)
      "Returns an instance of SOURCE with the same shape as OBJECT, where
      each element maps to itself.")

(setf (documentation #'generic-intersect 'function)
      "Returns an instance of INDEX-SPACE over the elements that are both
      in the domain of object-1 and object-2. Returns false if OBJECT-1 and
      OBJECT-2 have no common keys.")

(setf (documentation #'generic-invert 'function)
      "Returns the inverse function of TRANSFORMATION, such that the
      composition of TRANSFORMATION and (GENERIC-INVERT TRANSFORMATION)
      does nothing.")

(setf (documentation #'generic-reduce 'function)
      "Returns an instance of TOTAL-FUNCTION that is obtained by reducing the
      elements of the trailing dimension of OBJECT by successive
      applications of the binary operator OPERATOR. Signals an error if
      OBJECT has dimension zero.")

(setf (documentation #'generic-repeat 'function)
      "Returns an instance of TOTAL-FUNCTION with the same shape as SPACE and with
      the values of OBJECT as if multiple copies of OBJECT were translated
      via GENERIC-TRANSFORM so that they could be combined via
      GENERIC-FUSE. Signals an error is no suitable repetition is found.")

(setf (documentation #'generic-select 'function)
      "Returns an instance of TOTAL-FUNCTION that contains all elements of OBJECT
      that are denoted by SPACE. Signals an error if SPACE does not denote
      a proper subspace of OBJECT.")

(setf (documentation #'generic-size 'function)
      "Returns the number of relations of OBJECT.")

(setf (documentation #'generic-source 'function)
      "Returns an instance of SOURCE by dispatching on OBJECT-OR-SYMBOL,
      with further customization according to ARGUMENTS.")

(setf (documentation #'generic-target 'function)
      "Returns an instance of TARGET that describes how to store the
      content of OBJECT in the target described TARGET-OR-SYMBOL and
      ARGUMENTS.")

(setf (documentation #'generic-transform 'function)
      "Returns an instance of TOTAL-FUNCTION that maps each X to the value
      of OBJECT of TRANSFORMATION of X.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; default behavior

(defmethod generic-apply :before ((operator total-function) (object total-function)
                                  &rest more-objects)
  (assert (= (generic-dimension operator) (1+ (length more-objects))))
  (assert (not (find (generic-index-space object)
                     (mapcar #'generic-index-space more-objects)
                     :test (complement #'generic-equalp)))))

(defmethod generic-reduce :before ((operator total-function) (object total-function))
  (assert (< 1 (generic-dimension object))))

(defmethod generic-select ((object total-function) (space total-function))
  (assert (generic-equalp (generic-index-space space)
                          (generic-intersect object space))))

(defmethod generic-source ((object source) &rest arguments)
  (declare (ignore arguments))
  object)

(defmethod generic-fuse ((object total-function) &rest more-objects)
  (assert (apply #'= (mapcar #'generic-dimension
                             (list* object more-objects)))))
