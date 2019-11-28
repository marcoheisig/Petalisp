;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Network Parameters

(defclass network-parameter ()
  ((%name :initarg :name :reader network-parameter-name)
   (%value :initarg :value :reader network-parameter-value))
  (:default-initargs :value nil))

(defclass network-input (abstract-immediate network-parameter)
  ())

(defgeneric network-input-p (object)
  (:method ((object t)) nil)
  (:method ((object network-input)) t))

(defun make-network-input (shape ntype &key (name (gensym)))
  (if (null shape)
      (empty-array)
      (make-instance 'network-input
        :name name
        :shape shape
        :ntype ntype)))

(defclass network-weights (abstract-immediate network-parameter)
  ())

(defgeneric network-weights-p (object)
  (:method ((object t)) nil)
  (:method ((object network-weights)) t))

(defun make-network-weights (array &key (name (gensym)))
  (if (zerop (array-total-size array))
      (empty-array)
      (let ((lazy-array (coerce-to-lazy-array array)))
        (make-instance 'network-weights
          :name name
          :shape (shape lazy-array)
          :ntype (element-ntype lazy-array)
          :value lazy-array))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Network Class

(defclass network ()
  ((%weights :initarg :weights :reader network-weights)
   (%inputs :initarg :inputs :reader network-inputs)
   (%outputs :initarg :outputs :reader network-outputs)
   ;; An alist whose keys are backends and whose values are functions that
   ;; encapsulate the execution of that network on the corresponding
   ;; backend.
   (%compile-cache :initform '() :accessor network-compile-cache)))

(defun make-network (inputs outputs)
  (dolist (input inputs)
    (assert (network-input-p input)))
  (multiple-value-bind (derived-inputs weights)
      (derive-network-inputs-and-weights outputs)
    (let ((extra-inputs (set-difference derived-inputs inputs))
          (unused-inputs (set-difference inputs derived-inputs)))
      (cond (extra-inputs
             (error "~@<The network contains ~R inputs ~
                        that have not been explicitly specified. ~
                        The erroneous inputs are:~%~{ ~S~%~}~:@>"
                    (length extra-inputs)
                    extra-inputs))
            (unused-inputs
             (warn "~@<The network has ~R inputs that are never ~
                       used.  The unused inputs are:~%~{ ~S~%~}~:@>"
                   (length unused-inputs)
                   unused-inputs))))
    (make-instance 'network
      :inputs inputs
      :outputs outputs
      :weights weights)))

(defun derive-network-inputs-and-weights (outputs)
  (let ((table (make-hash-table :test #'eq))
        (weights '())
        (inputs '())
        (computable '()))
    (labels ((scan (lazy-array)
               (cond ((= 1 (refcount lazy-array))
                      (process lazy-array))
                     ((not (gethash lazy-array table))
                      (setf (gethash lazy-array table) t)
                      (process lazy-array))))
             (process (lazy-array)
               (cond ((typep lazy-array 'network-input)
                      (push lazy-array inputs))
                     ((typep lazy-array 'network-weights)
                      (push lazy-array weights))
                     ((computablep lazy-array)
                      (push lazy-array computable))
                     (t
                      (mapc #'scan (inputs lazy-array))))))
      (mapc #'scan outputs))
    (apply #'compute computable)
    (values inputs weights)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Automatic Differentiation

(defstruct (ad-record
            (:constructor make-ad-record
                (lazy-array
                 &aux (input-gradient-caches (make-array (length (inputs lazy-array))))))
            (:copier nil)
            (:predicate nil))
  (lazy-array nil :type lazy-array :read-only t)
  ;; An alist with one (argument-index . a-d-record) entry per reference to
  ;; this record's lazy array.
  (alist nil :type list)
  ;; The output gradient of this record's lazy-array.
  (output-gradient-cache nil :type (or null lazy-array))
  ;; A vector whose elements are either NIL, or lazy arrays that describe
  ;; the gradient at the corresponding input of this record's lazy array.
  (input-gradient-caches nil :type vector))

(defmacro ad-record-input-gradient-cache (ad-record index)
  `(svref (ad-record-input-gradient-caches ,ad-record) ,index))

(defun network-gradients (network output-gradients)
  (let ((table (make-hash-table :test #'eq)))
    ;; Populate the automatic differentiation table.
    (let ((worklist (network-outputs network)))
      (loop while worklist do
        (let ((lazy-array (pop worklist)))
          (unless (gethash lazy-array table)
            (setf (gethash lazy-array table)
                  (make-ad-record lazy-array))
            (loop for input in (inputs lazy-array) do
              (push input worklist))))))
    ;; Determine the outputs of each automatic differentiation record.
    (maphash
     (lambda (lazy-array record)
       (loop for input in (inputs lazy-array)
             for index from 0 do
               (push (cons index (gethash input table))
                     (ad-record-alist record))))
     table)
    ;; Set the output gradients of all network outputs.
    (loop for output in (network-outputs network)
          for gradient in output-gradients do
      (setf (ad-record-output-gradient-cache (gethash output table))
            gradient))
    ;; Determine the gradients of all records reachable from the weights.
    (loop for weights in (network-weights network)
          collect
          (ad-record-output-gradient (gethash weights table)))))

(defun ad-record-output-gradient (ad-record)
  (let ((cached-value (ad-record-output-gradient-cache ad-record)))
    (if (not (null cached-value))
        cached-value
        (let* ((alist (ad-record-alist ad-record))
               (gradients
                 (loop for (index . record) in alist
                       collect
                       (ad-record-input-gradient record index))))
          (setf (ad-record-output-gradient-cache ad-record)
                (apply #'α #'+ gradients))))))

(defun ad-record-input-gradient (ad-record index)
  (let ((cached-value (ad-record-input-gradient-cache ad-record index)))
    (if (not (null cached-value))
        cached-value
        (setf (ad-record-input-gradient-cache ad-record index)
              (input-gradient
               (ad-record-lazy-array ad-record)
               (ad-record-output-gradient ad-record)
               index)))))

(defgeneric input-gradient (lazy-array output-gradient index))

(defmethod input-gradient ((application application) (output-gradient lazy-array) index)
  (with-accessors ((inputs inputs)
                   (operator operator)
                   (shape shape)) application
    (α #'*
       (nth index inputs)
       (petalisp.type-inference:differentiate
        operator
        inputs
        #'element-ntype
        (lambda (constant)
          (reshape constant shape))
        (lambda (ntypes function inputs)
          (make-instance 'application
            :operator function
            :value-n 0
            :inputs inputs
            :shape shape
            :ntype (elt ntypes 0)))
        index))))

(defmethod input-gradient ((reduction reduction) (output-gradient lazy-array) index)
  (break "TODO"))

(defmethod input-gradient ((fusion fusion) (output-gradient lazy-array) index)
  (reshape output-gradient (shape (nth index (inputs fusion)))))

(defmethod input-gradient ((reference reference) (output-gradient lazy-array) (index (eql 0)))
  (with-accessors ((transformation transformation)
                   (shape shape)) reference
    (if (transformation-invertiblep transformation)
        (make-reference
         output-gradient
         (shape reference)
         (invert-transformation (transformation reference)))
        (map-transformation-outputs
         (lambda ())
         transformation))))
