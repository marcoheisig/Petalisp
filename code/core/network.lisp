;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Network Inputs

(defclass network-input (abstract-immediate)
  ((%name :initarg :name :reader network-input-name)))

(defgeneric network-input-p (object)
  (:method ((object t)) nil)
  (:method ((object network-input)) t))

(defun make-network-input (shape &key element-type (name (gensym)))
  (if (null shape)
      (empty-array)
      (make-instance 'network-input
        :name name
        :shape shape
        :ntype (petalisp.type-inference:ntype element-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Network Class

(defclass network ()
  ((%inputs :initarg :inputs :reader network-inputs)
   (%outputs :initarg :outputs :reader network-outputs)
   ;; An alist whose keys are backends and whose values are functions that
   ;; encapsulate the execution of that network on the corresponding
   ;; backend.
   (%compile-cache :initform '() :accessor network-compile-cache)))

(defun make-network (&key inputs outputs)
  (dolist (input inputs)
    (assert (network-input-p input)))
  (multiple-value-bind (derived-inputs) (derive-network-inputs outputs)
    (let ((extra-inputs (set-difference derived-inputs inputs))
          (unused-inputs (set-difference inputs derived-inputs)))
      (cond (extra-inputs
             (error "~@<The network contains ~R inputs ~
                        that have not been explicitly specified. ~
                        The erroneous inputs are:~%~{ ~S~%~}~:@>"
                    (length extra-inputs)
                    extra-inputs))
            (unused-inputs
             (loop for unused-input in unused-inputs do
               (warn "~@<The ~:R input of the network is never used.~:@>"
                     (1+ (position unused-input inputs)))))))
    (make-instance 'network
      :inputs inputs
      :outputs outputs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Automatic Differentiation.

(defun derive-network-inputs (outputs)
  (let ((table (make-hash-table :test #'eq))
        (inputs '()))
    (labels ((scan (lazy-array)
               (cond ((= 1 (refcount lazy-array))
                      (process lazy-array))
                     ((not (gethash lazy-array table))
                      (setf (gethash lazy-array table) t)
                      (process lazy-array))))
             (process (lazy-array)
               (cond ((network-input-p lazy-array)
                      (push lazy-array inputs))
                     (t
                      (mapc #'scan (inputs lazy-array))))))
      (mapc #'scan outputs))
    inputs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Automatic Differentiation

(defstruct (ad-record
            (:constructor make-ad-record
                (lazy-array
                 &aux (input-gradient-caches
                       (make-array (length (inputs lazy-array)) :initial-element nil))))
            (:copier nil)
            (:predicate nil))
  (name (gensym))
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

(defun gradient-network (network)
  (let ((table (make-hash-table :test #'eq)))
    ;; Populate the automatic differentiation table.
    (labels ((ensure-ad-record (lazy-array)
               (unless (gethash lazy-array table)
                 (setf (gethash lazy-array table)
                       (make-ad-record lazy-array))
                 (mapc #'ensure-ad-record (inputs lazy-array)))))
      (mapc #'ensure-ad-record (network-outputs network))
      (mapc #'ensure-ad-record (network-inputs network)))
    ;; Determine the outputs of each automatic differentiation record.
    (maphash
     (lambda (lazy-array record)
       (loop for input in (inputs lazy-array)
             for input-ad-record = (gethash input table)
             for index from 0 do
               (push (cons index record)
                     (ad-record-alist input-ad-record))))
     table)
    (let ((training-outputs
            (loop for output in (network-outputs network)
                  collect
                  (make-network-input
                   (shape output)
                   :element-type
                   (petalisp.type-inference:type-specifier
                    (petalisp.type-inference:generalize-ntype
                     (element-ntype output)))))))
      ;; Set the output gradients of all network outputs.
      (loop for network-output in (network-outputs network)
            for training-output in training-outputs do
              (setf (ad-record-output-gradient-cache (gethash network-output table))
                    (α #'- network-output training-output)))
      ;; Create a new network that will map inputs and outputs to gradients.
      (make-network
       :inputs (append (network-inputs network) training-outputs)
       :outputs (loop for input in (network-inputs network)
                      collect (ad-record-output-gradient (gethash input table)))))))

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
                (α #'*
                   (ad-record-lazy-array ad-record)
                   (apply #'α #'+ gradients)))))))

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
       output-gradient
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
