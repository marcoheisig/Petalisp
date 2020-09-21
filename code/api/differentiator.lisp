;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defstruct (ad-record
            (:constructor make-ad-record
                (lazy-array
                 &aux (input-gradient-caches
                       (make-array (length (inputs lazy-array)) :initial-element nil))))
            (:copier nil)
            (:predicate nil))
  (lazy-array nil :type lazy-array :read-only t)
  ;; An alist with one (argument-index . a-d-record) entry per reference to
  ;; this record's lazy array.
  (alist nil :type list)
  ;; The output gradient of this record's lazy-array.
  (output-gradient-cache nil :type (or null lazy-array))
  ;; Each input gradient cache is either NIL, or a lazy array that
  ;; describes the gradient at the corresponding input of this record's
  ;; lazy array.
  (input-gradient-caches nil :type vector))

(defmethod print-object ((ad-record ad-record) stream)
  (print-unreadable-object (ad-record stream :type t :identity t)
    (format stream "~S" (ad-record-lazy-array ad-record))))

(defmacro ad-record-input-gradient-cache (ad-record index)
  `(svref (ad-record-input-gradient-caches ,ad-record) ,index))

(defun differentiator (outputs gradients)
  (unless (= (length outputs) (length gradients))
    (error "~@<Differentiation requires exactly as many gradients ~
               as there are outputs.~:@>"))
  (let ((table (make-hash-table :test #'eq)))
    ;; Populate the table with ad-records.
    (labels ((ensure-ad-record (lazy-array)
               (unless (gethash lazy-array table)
                 (setf (gethash lazy-array table)
                       (make-ad-record lazy-array))
                 (mapc #'ensure-ad-record (inputs lazy-array)))))
      (mapc #'ensure-ad-record outputs))
    ;; Connect all ad-records.
    (maphash
     (lambda (lazy-array record)
       (loop for input in (inputs lazy-array)
             for input-ad-record = (gethash input table)
             for index from 0 do
               (push (cons index record)
                     (ad-record-alist input-ad-record))))
     table)
    ;; Set the output gradients of all outputs.
    (loop for output in outputs
          for gradient in gradients do
            (setf (ad-record-output-gradient-cache (gethash output table))
                  (typecase gradient
                    (symbol
                     (make-instance 'parameter
                       :name gradient
                       :shape (array-shape output)
                       :ntype (element-ntype output)))
                    (t
                     (reshape
                      (α 'coerce gradient
                         (petalisp.type-inference:type-specifier
                          (petalisp.type-inference:generalize-ntype
                           (element-ntype output))))
                      (array-shape output))))))
    ;; Return the two differentiating closures.
    (labels ((ad-record (lazy-array)
               (check-type lazy-array lazy-array)
               (multiple-value-bind (ad-record present-p)
                   (gethash lazy-array table)
                 (unless present-p
                   (error "~@<The lazy array ~S is not part of the ~
                      previously differentiated graph.~:@>"
                          lazy-array))
                 ad-record)))
      (values
       (lambda (lazy-array)
         (ad-record-output-gradient
          (ad-record (lazy-array lazy-array))))
       (lambda (lazy-array index)
         (ad-record-input-gradient
          (ad-record (lazy-array lazy-array))
          index))))))

(defun ad-record-output-gradient (ad-record)
  (let* ((cached-value (ad-record-output-gradient-cache ad-record)))
    (if (not (null cached-value))
        cached-value
        (let* ((lazy-array (ad-record-lazy-array ad-record))
               (alist (ad-record-alist ad-record))
               (gradients
                 (list*
                  (reshape (coerce 0 (element-type lazy-array)) (array-shape lazy-array))
                  (loop for (index . record) in alist
                        collect
                        (ad-record-input-gradient record index)))))
          (setf (ad-record-output-gradient-cache ad-record)
                (apply
                 #'fuse
                 (loop for (shape . bitmask) in (subdivide-arrays gradients)
                       collect
                       (apply
                        #'α #'+
                        (loop for gradient in gradients
                              for index from 0
                              when (logbitp index bitmask)
                                collect (reshape gradient shape))))))))))

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

(defmethod input-gradient :around
    (lazy-array output-gradient index)
  (let ((input (nth index (inputs lazy-array)))
        (value (call-next-method)))
    (if (petalisp.type-inference:ntype=
         (element-ntype value)
         (element-ntype input))
        value
        (α #'coerce value (element-type input)))))

(defmethod input-gradient
    ((lazy-map lazy-map)
     (output-gradient lazy-array)
     index)
  (with-accessors ((inputs inputs)
                   (operator operator)
                   (shape array-shape)) lazy-map
    (α #'*
       output-gradient
       (petalisp.type-inference:differentiate
        operator
        inputs
        #'element-ntype
        (lambda (constant)
          (reshape constant shape))
        (lambda (ntypes function inputs)
          (make-instance 'lazy-map
            :operator function
            :inputs inputs
            :shape shape
            :ntype (elt ntypes 0)))
        index))))

(defmethod input-gradient
    ((lazy-fuse lazy-fuse) (output-gradient lazy-array) index)
  (reshape output-gradient (array-shape (nth index (inputs lazy-fuse)))))

(defun move-axis-to-front (array axis)
  (check-type axis rank)
  (reshape array
           (make-transformation
            :output-mask
            (loop for index below (rank array)
                  collect
                  (cond
                    ((= index 0) axis)
                    ((<= index axis) (1- index))
                    ((> index axis) index))))))

(defmethod input-gradient
    ((lazy-reshape lazy-reshape)
     (output-gradient lazy-array)
     (index (eql 0)))
  (with-accessors ((transformation transformation)
                   (shape array-shape)) lazy-reshape
    (if (transformation-invertiblep transformation)
        (reshape output-gradient (transformation lazy-reshape))
        ;; The input gradient of a broadcasting reference is the sum of all
        ;; incoming gradients.  We do so by summing the gradients along
        ;; each broadcast axis, and by replacing each corresponding input
        ;; mask entry of the broadcast transformation by zero.  This way,
        ;; we turn the non-invertible broadcast transformation into an
        ;; invertible transformation that we can use to reshape the summed
        ;; up gradients to the desired shape.
        (let ((input-mask (copy-seq (petalisp.core:transformation-input-mask transformation))))
          (petalisp.core:map-transformation-inputs
           (lambda (input-index input-constraint output-index)
             (declare (ignore input-constraint))
             (when (null output-index)
               (setf output-gradient (sum-axis output-gradient input-index))
               (setf (elt input-mask input-index) 0)))
           transformation)
          (reshape
           output-gradient
           (make-transformation
            :input-mask input-mask
            :output-mask (petalisp.core:transformation-output-mask transformation)
            :offsets (petalisp.core:transformation-offsets transformation)))))))

(defun sum-axis (array axis)
  (reshape
   (β #'+ (move-axis-to-front array axis))
   (make-transformation
    :input-rank (1- (rank array))
    :output-mask
    (loop for index below (rank array)
          collect
          (cond ((< index axis) index)
                ((= index axis) nil)
                ((> index axis) (1- index)))))))
