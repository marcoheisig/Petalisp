;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defstruct (ad-record
            (:constructor make-ad-record
                (lazy-array
                 &aux (input-gradient-caches
                       (make-array (length (lazy-array-inputs lazy-array))
                                   :initial-element nil))))
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
                 (mapc #'ensure-ad-record (lazy-array-inputs lazy-array)))))
      (mapc #'ensure-ad-record outputs))
    ;; Connect all ad-records.
    (maphash
     (lambda (lazy-array record)
       (loop for input in (lazy-array-inputs lazy-array)
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
                       :shape (lazy-array-shape output)
                       :ntype (lazy-array-ntype output)))
                    (t
                     (lazy-reshape
                      (lazy 'coerce gradient
                            (petalisp.type-inference:type-specifier
                             (petalisp.type-inference:generalize-ntype
                              (lazy-array-ntype output))))
                      (lazy-array-shape output))))))
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
                  (lazy-reshape
                   (coerce 0 (lazy-array-element-type lazy-array))
                   (lazy-array-shape lazy-array))
                  (loop for (index . record) in alist
                        collect
                        (ad-record-input-gradient record index)))))
          (setf (ad-record-output-gradient-cache ad-record)
                (apply
                 #'lazy-fuse
                 (loop for (shape . bitmask) in (subdivide-arrays gradients)
                       collect
                       (apply
                        #'lazy #'+
                        (loop for gradient in gradients
                              for index from 0
                              when (logbitp index bitmask)
                                collect (lazy-reshape gradient shape))))))))))

(defun ad-record-input-gradient (ad-record index)
  (let ((cached-value (ad-record-input-gradient-cache ad-record index)))
    (if (not (null cached-value))
        cached-value
        (let* ((lazy-array (ad-record-lazy-array ad-record))
               (delayed-action (lazy-array-delayed-action lazy-array)))
          (setf (ad-record-input-gradient-cache ad-record index)
                (input-gradient
                 lazy-array delayed-action
                 (ad-record-output-gradient ad-record)
                 index))))))

(defun coerce-to-ntype (lazy-array ntype)
  (if (petalisp.type-inference:ntype= (lazy-array-ntype lazy-array) ntype)
      lazy-array
      (lazy #'coerce lazy-array (petalisp.type-inference:type-specifier ntype))))

(defgeneric input-gradient (lazy-array delayed-action output-gradient index))

(defmethod input-gradient
    ((lazy-array lazy-array)
     (delayed-map delayed-map)
     (output-gradient lazy-array)
     index)
  (let ((operator (delayed-map-operator delayed-map))
        (inputs (delayed-map-inputs delayed-map))
        (shape (lazy-array-shape lazy-array)))
    (coerce-to-ntype
     (lazy #'*
           output-gradient
           (petalisp.type-inference:differentiate
            operator
            inputs
            #'lazy-array-ntype
            (lambda (constant)
              (lazy-reshape constant shape))
            (lambda (ntypes function inputs)
              (coerce-to-ntype
               (apply #'lazy function inputs)
               (elt ntypes 0)))
            index))
     (lazy-array-ntype (nth index inputs)))))

(defmethod input-gradient
    ((lazy-array lazy-array)
     (delayed-fuse delayed-fuse)
     (output-gradient lazy-array)
     index)
  (let ((input (nth index (delayed-fuse-inputs delayed-fuse))))
    (coerce-to-ntype
     (lazy-reshape output-gradient (lazy-array-shape input))
     (lazy-array-ntype input))))

(defun move-axis-to-front (array axis)
  (let* ((lazy-array (lazy-array array))
         (rank (lazy-array-rank lazy-array)))
    (unless (and (integerp axis) (< -1 axis rank))
      (error "Invalid axis ~S for array ~S." axis array))
    (lazy-reshape
     lazy-array
     (make-transformation
      :output-mask
      (loop for index below rank
            collect
            (cond
              ((= index 0) axis)
              ((<= index axis) (1- index))
              ((> index axis) index)))))))

(defmethod input-gradient
    ((lazy-array lazy-array)
     (delayed-reshape delayed-reshape)
     (output-gradient lazy-array)
     (index (eql 0)))
  (let ((transformation (delayed-reshape-transformation delayed-reshape)))
    (if (transformation-invertiblep transformation)
        (lazy-reshape output-gradient transformation)
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
          (lazy-reshape
           output-gradient
           (make-transformation
            :input-mask input-mask
            :output-mask (petalisp.core:transformation-output-mask transformation)
            :offsets (petalisp.core:transformation-offsets transformation)))))))

(defun sum-axis (array axis)
  (let ((lazy-array (lazy-array array)))
    (lazy-reshape
     (lazy-reduce #'+ (move-axis-to-front lazy-array axis))
     (make-transformation
      :input-rank (1- (lazy-array-rank lazy-array))
      :output-mask
      (loop for index below (lazy-array-rank lazy-array)
            collect
            (cond ((< index axis) index)
                  ((= index axis) nil)
                  ((> index axis) (1- index))))))))
