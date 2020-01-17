;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

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
  ;; Each input gradient cache is either NIL, or a lazy array that
  ;; describes the gradient at the corresponding input of this record's
  ;; lazy array.
  (input-gradient-caches nil :type vector))

(defmacro ad-record-input-gradient-cache (ad-record index)
  `(svref (ad-record-input-gradient-caches ,ad-record) ,index))

(defun differentiate (outputs gradients)
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
                     (make-parameter
                      gradient
                      :shape (shape output)
                      :element-type (element-ntype output)))
                    (t
                     (reshape
                      (α 'coerce gradient
                         (petalisp.type-inference:type-specifier
                          (petalisp.type-inference:generalize-ntype
                           (element-ntype output))))
                      (shape output))))))
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
          (ad-record lazy-array)))
       (lambda (lazy-array index)
         (ad-record-input-gradient
          (ad-record lazy-array)
          index))))))

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

(defmethod input-gradient ((reference reference) (output-gradient lazy-array) (index (eql 0)))
  (with-accessors ((transformation transformation)
                   (shape shape)) reference
    (if (transformation-invertiblep transformation)
        (make-reference
         output-gradient
         (shape reference)
         (invert-transformation (transformation reference)))
        ;; The input gradient of a broadcasting reference is the average of
        ;; all incoming gradients.
        (let ((axes '()))
          (map-transformation-inputs
           (lambda (input-index input-constraint output-index)
             (declare (ignore input-constraint))
             (when (null output-index)
               (push input-index axes)))
           transformation)
          (labels ((average (array axes)
                     (if (null axes)
                         array
                         (let ((array (move-axis-to-front array (first axes))))
                           (average
                            (α #'/
                               (β #'+ array)
                               (range-size (first (shape-ranges (shape array)))))
                            (rest axes))))))
            (average output-gradient axes))))))
