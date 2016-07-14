;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defclass total-function ()
  ((%domain-type :initarg :domain-type :reader domain-type)
   (%codomain-type :initarg :codomain-type :reader codomain-type)))

(defclass application (total-function)
  ((%operator :initarg :operator :reader operator)
   (%objects :initarg :objects :reader objects)))

(defclass reduction (total-function)
  ((%operator :initarg :operator :reader operator)
   (%object :initarg :object :reader object)))

(defclass repetition (total-function)
  ((%object :initarg :object :reader object)))

(defclass fusion (total-function)
  ((%objects :initarg :objects :reader objects)))

(defclass selection (total-function)
  ((%object :initarg :object :reader object)))

(defclass transformation (total-function)
  ((%object :initarg :object :reader object)))

(defclass source (total-function) ()
  (:documentation
   "A total function that is determined by some kind of immutable source."))

(defclass target (total-function) ()
  (:documentation
   "A description of how to store the value of a given total function."))

(defclass index-space (source) ()
  (:documentation
   "A total function that maps every element of the domain of another total
   function to itself."))

(defclass affine-transformation (transformation)
  ((%coefficients :initarg :coefficients :reader coefficients)))

(defclass permutation (transformation)
  ((%permutation :initarg :permutation :reader permutation)))

(defclass affine-permutation (affine-transformation permutation) ())
