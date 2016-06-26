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
  "Use LISP-OBJECT to create a Petalisp object. Behaves like IDENTITY when
LISP-OBJECT is already a Petalisp object."
  (multiple-value-bind (index-space element-type)
      (cond
        ((typep lisp-object 'petalisp-object)
         (return-from make-lisp-input lisp-object))
        ((arrayp lisp-object)
         (values
          (mapcar (lambda (dim) (range 1 1 dim))
                  (array-dimensions lisp-object))
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
           (apply #'extend-dimensions
                  (mapcar #'make-lisp-input arguments)))
         (element-type
           (apply #'result-type operator
                  (mapcar #'element-type arguments)))
         (index-space
           (index-space (first arguments))))
    (make-instance
     'α
     :operator operator
     :index-space index-space
     :element-type element-type)))

(defun extend-dimensions (&rest petalisp-objects)
  "Broadcasting operations in Petalisp require that all arguments have the
  same underlying index space. This function attempts to extend the
  dimensions of each argument similarly to the broadcasting rules found in
  other programming toolkits like NumPy."
  (let* ((index-spaces (mapcar #'index-space petalisp-objects))
         (result-space (apply #'mapcar #'extend-dimensions-1D index-spaces)))
    ;; upgrade arguments where necessary
    (mapcar
     (lambda (x) (replicate x result-space))
     petalisp-objects)))

(defun extend-dimensions-1D (&rest ranges)
  (let ((big-ranges
          (if (every #'unary-range-p ranges)
              ranges
              (remove-if #'unary-range-p ranges))))
    (unless (apply #'range= big-ranges)
      (error 'dimensions-not-compatible :ranges ranges))
    (first big-ranges)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; β - reduction of the trailing dimension

(defun β (operator object)
  (let ((result-space (nreverse (cdr (reverse (index-space object)))))
        (element-type (element-type object)))
    (assert (petalisp-subtypep
             (result-type operator element-type element-type)
             element-type))
    (make-instance
     'β
     :operator operator
     :index-space result-space
     :element-type element-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; replicate - fill a certain shape with copies of an object

(defun replicate (object result-space)
  "Returns a Petalisp object with the shape RESULT-SPACE that is obtained
by replicating the content of object enough times. An error is thrown if
there is a dimension where the number of elements in RESULT-SPACE is not
divisible by the number of elements in OBJECT."
  (cond
    ((index-space= result-space (index-space object))
     (return-from replicate object))
    ;; TODO
    ))

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
