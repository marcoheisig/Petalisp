;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defgeneric compile-network-on-backend (network backend))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Network Class

(defclass network ()
  ((%parameters :initarg :parameters :reader network-parameters)
   (%outputs :initarg :outputs :reader network-outputs)))

(defun make-network (&key parameters outputs)
  (loop for (parameter . more-parameters) on parameters do
    (loop for other-parameter in more-parameters do
      (assert (not (eq (parameter-name parameter)
                       (parameter-name other-parameter))))))
  (multiple-value-bind (derived-parameters) (derive-parameters outputs)
    (let ((extra-parameters (set-difference derived-parameters parameters))
          (unused-parameters (set-difference parameters derived-parameters)))
      (cond (extra-parameters
             (error "~@<The network contains ~R parameters ~
                        that have not been explicitly specified. ~
                        The erroneous parameters are:~%~{ ~S~%~}~:@>"
                    (length extra-parameters)
                    extra-parameters))
            (unused-parameters
             (loop for unused-input in unused-parameters do
               (warn "~@<The ~:R input of the network is never used.~:@>"
                     (1+ (position unused-input parameters)))))))
    (make-instance 'network
      :parameters parameters
      :outputs outputs)))

(defun derive-parameters (outputs)
  (let ((table (make-hash-table :test #'eq))
        (parameters '()))
    (labels ((scan (lazy-array)
               (cond ((= 1 (refcount lazy-array))
                      (process lazy-array))
                     ((not (gethash lazy-array table))
                      (setf (gethash lazy-array table) t)
                      (process lazy-array))))
             (process (lazy-array)
               (cond ((typep lazy-array 'parameter)
                      (push lazy-array parameters))
                     (t
                      (mapc #'scan (inputs lazy-array))))))
      (mapc #'scan outputs))
    parameters))

(defun call-network (network &rest plist &key &allow-other-keys)
  (let ((args
          (loop for parameter in (network-parameters network)
                for name = (parameter-name parameter)
                for value = (getf plist name '.missing.)
                collect (if (eq value '.missing.)
                            (error "Missing input: ~S." name)
                            value))))
    (apply (compile-network-on-backend network *backend*) args)))

;;; This is a simple, albeit slow way of compiling a network.  We simply
;;; return a closure that substitutes all networks parameters with the provided
;;; values and then computes that new graph.
(defmethod compile-network-on-backend
    ((network network) (backend backend))
  (lambda (&rest args)
    (compute-on-backend
     (substitute-arrays
      (network-outputs network)
      args
      (network-parameters network))
     backend)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Network Differentiation

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

(defun default-gradient-name (n)
  (values (intern (format nil "GRADIENT-~D" n) :keyword)))

(defun gradient-network (network &key (gradient-names #'default-gradient-name))
  (let ((table (make-hash-table :test #'eq))
        (nth-gradient-name
          (if (listp gradient-names)
              (lambda (n) (elt n gradient-names))
              gradient-names)))
    ;; Populate the automatic differentiation table.
    (labels ((ensure-ad-record (lazy-array)
               (unless (gethash lazy-array table)
                 (setf (gethash lazy-array table)
                       (make-ad-record lazy-array))
                 (mapc #'ensure-ad-record (inputs lazy-array)))))
      (mapc #'ensure-ad-record (network-outputs network))
      (mapc #'ensure-ad-record (network-parameters network)))
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
                  for n from 0
                  collect
                  (make-parameter
                   (funcall nth-gradient-name n)
                   :shape (shape output)
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
       :parameters (append (network-parameters network) training-outputs)
       :outputs (loop for parameter in (network-parameters network)
                      collect (ad-record-output-gradient (gethash parameter table)))))))

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
        (break "TODO")
        #+(or)
        (let ((gradient 0))
          (map-transformation-outputs
           (lambda (output-index input-index scaling offset)
             (declare (ignore offset))
             (if (zerop scaling)
                 (setf gradient (β #'+ output-gradient))))
           transformation)))))
