;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Petalisp Core

(in-package :petalisp)

(defclass petalisp-object ()
  ((%index-space :initarg :index-space :reader index-space)
   (%element-type :initarg :element-type :reader element-type))
  (:documentation
   "The superclass of all objects handled by Petalisp."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; input - a protocol to generate Petalisp objects

(defclass input (petalisp-object) ()
  (:documentation
   "The superclass of all Petalisp leaf nodes, that is Petalisp objects that
  are not the result of applying an elementary Petalisp function to other
  Petalisp objects."))

(defclass constant-input (input)
  ((%value :initarg :value :reader value))
  (:documentation
   "A Petalisp object that returns the same value for all indices."))

(defclass index-input (input) ()
  (:documentation
   "A Petalisp object that where each value equals its index."))

(defclass lisp-input (input)
  ((%lisp-object :initarg :lisp-object :reader lisp-object))
  (:documentation
   "A Petalisp object that is constructed from a given lisp constant. A lisp
  array will be used to build a Petalisp object with the same dimensions
  and elements. All other objects are transformed to a Petalisp object
  that contains exactly this one element."))

(defun make-lisp-input (lisp-object)
  "Use LISP-OBJECT to create a Petalisp object. Behaves like IDENTITY if
LISP-OBJECT is already a Petalisp object."
  (multiple-value-bind (index-space element-type)
      (cond
        ((typep lisp-object 'petalisp-object)
         (return-from make-lisp-input lisp-object))
        ((arrayp lisp-object)
         (values
          (apply #'make-index-space
                 (mapcar (lambda (dim) (range 0 1 (- dim 1)))
                         (array-dimensions lisp-object)))
          (petalispify-type
           (array-element-type lisp-object))))
        (t
         (values
          (x)
          (petalispify-type (type-of lisp-object)))))
    (make-instance
     'lisp-input
     :lisp-object lisp-object
     :index-space index-space
     :element-type element-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; α - distributed function application

(defclass α (petalisp-object)
  ((%operator :initarg :operator :reader operator)
   (%arguments :initarg :arguments :reader arguments)))

(defun α (operator &rest arguments)
  (let* ((arguments
           (mapcar #'make-lisp-input arguments))
         (index-space
           (apply #'index-space-broadcast
                  (mapcar #'index-space arguments)))
         (arguments
           (mapcar
            (lambda (argument)
              (replicate argument index-space))
            arguments))
         (element-type
           (apply #'result-type operator
                  (mapcar #'element-type arguments))))
    (make-instance
     'α
     :operator operator
     :arguments arguments
     :index-space index-space
     :element-type element-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; β - reduction of the trailing dimension

(defclass β (petalisp-object)
  ((%operator :initarg :operator :reader operator)
   (%argument :initarg :argument :reader argument)))

(defun β (operator argument)
  (let ((result-space
          (index-space-drop-last-dimension
           (index-space argument)))
        (element-type (element-type argument)))
    (assert (petalisp-subtypep
             (result-type operator element-type element-type)
             element-type))
    (make-instance
     'β
     :operator operator
     :argument argument
     :index-space result-space
     :element-type element-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; replicate - fill a certain shape with copies of an object

(defclass replicate (petalisp-object)
  ((%argument :initarg :argument :reader argument)))

(defun replicate (argument result-space)
  "Returns a Petalisp object with the shape RESULT-SPACE that is obtained
by replicating the content of object enough times. An error is signaled if
there is a dimension where the number of elements in RESULT-SPACE is not
divisible by the number of elements in OBJECT."
  (when (index-space= result-space (index-space argument))
    (return-from replicate argument))
  (unless (every #'integerp
                 (mapcar
                  (lambda (src-range dst-range)
                    (/ (range-elements dst-range)
                       (range-elements src-range)))
                  (index-space argument)
                  result-space))
    (error 'unable-to-replicate
           :object argument
           :index-space result-space))
  (make-instance
   'replicate
   :argument argument
   :index-space result-space
   :element-type (element-type argument)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; select - take parts of an object

(defun select (shape object)
  ;; TODO
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; translate - change the index space

(defun translate (object ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; fuse

(defun fuse (&rest objects)
  ;; TODO
  objects)
