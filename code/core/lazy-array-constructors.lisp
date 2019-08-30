;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Empty Arrays

(defun empty-array ()
  (load-time-value
   (make-instance 'empty-array)))

(defun empty-arrays (n)
  (case n
    (0 (values))
    (1 (values (empty-array)))
    (2 (values (empty-array) (empty-array)))
    (otherwise (values-list (make-list n :initial-element (empty-array))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; References

(defgeneric make-reference (input shape transformation)
  (:argument-precedence-order transformation shape input))

;;; Compose consecutive references.
(defmethod make-reference ((reference reference)
                           (shape shape)
                           (transformation transformation))
  (make-reference
   (input reference)
   shape
   (compose-transformations
    (transformation reference)
    transformation)))

;;; Drop references with no effect.
(defmethod make-reference ((lazy-array lazy-array)
                           (shape shape)
                           (identity-transformation identity-transformation))
  (if (and (shape-equal (shape lazy-array) shape)
           ;; Don't drop references to range immediates.  The reason for
           ;; this is that we never want these immediates to appear as
           ;; roots of a data flow graph.
           (not (typep lazy-array 'range-immediate)))
      lazy-array
      (call-next-method)))

;;; Handle empty shapes.
(defmethod make-reference ((lazy-array lazy-array)
                           (null null)
                           (transformation transformation))
  (empty-array))

;;; The default - construct a new reference.
(defmethod make-reference ((lazy-array lazy-array)
                           (shape shape)
                           (transformation transformation))
  (make-instance 'reference
    :ntype (ntype lazy-array)
    :inputs (list lazy-array)
    :shape shape
    :transformation transformation))

;;; Error handling.
(defmethod make-reference :before
    ((lazy-array lazy-array) (shape shape) (transformation transformation))
  (let ((relevant-shape (transform shape transformation))
        (input-shape (shape lazy-array)))
    (unless (and (= (shape-rank relevant-shape) (shape-rank input-shape))
                 (subshapep relevant-shape input-shape))
      (error "~@<Invalid reference to ~S with shape ~S and transformation ~S.~:@>"
             lazy-array shape transformation))))

(defun reshape (array &rest shapes-and-transformations)
  (labels ((reshape-with-shape (lazy-array shape)
             (make-reference
              lazy-array
              shape
              (broadcasting-transformation shape (shape lazy-array))))
           (reshape-with-transformation (lazy-array transformation)
             (make-reference
              lazy-array
              (transform (shape lazy-array) transformation)
              (invert-transformation transformation)))
           (reshape1 (lazy-array modifier)
             (if (shapep modifier)
                 (reshape-with-shape lazy-array modifier)
                 (reshape-with-transformation lazy-array modifier))))
    (reduce #'reshape1 shapes-and-transformations :initial-value (coerce-to-lazy-array array))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fusions

(defun sanitize-fusion-inputs (inputs)
  (let ((lazy-arrays
          (loop for input in inputs
                unless (empty-array-p input)
                  collect (coerce-to-lazy-array input))))
    (unless (petalisp.utilities:identical lazy-arrays :key #'rank)
      (error
       "~@<The shapes of the arguments to a fusion operation must ~
         have the same rank, but the supplied arguments have the ~
         ranks ~{~#[~;~S~;~S and ~S~:;~@{~S~#[~;, and ~:;, ~]~}~]~}.~:@>"
       (remove-duplicates (mapcar #'rank lazy-arrays))))
    lazy-arrays))

(defun fuse (&rest inputs)
  (let ((lazy-arrays (sanitize-fusion-inputs inputs)))
    ;; When given more than one input, check for disjointnes.
    (when (cdr lazy-arrays)
      (map-combinations
       (lambda (two-inputs)
         (destructuring-bind (input-1 input-2) two-inputs
           (let ((shape-1 (shape input-1))
                 (shape-2 (shape input-2)))
             (assert (not (shape-intersectionp shape-1 shape-2)) ()
                     "~@<The index shapes of the arguments to a fusion operation ~
                         must be disjoint, but shape ~S and shape ~S have the ~
                         common subshape ~S.~:@>"
                     shape-1
                     shape-2
                     (shape-intersection shape-1 shape-2)))))
       lazy-arrays :length 2 :copy nil))
    (make-fusion lazy-arrays)))

(defun fuse* (&rest inputs)
  (let* ((lazy-arrays (sanitize-fusion-inputs inputs))
         (shapes (subdivision (mapcar #'shape lazy-arrays)))
         (identity (identity-transformation (rank (first lazy-arrays)))))
    (flet ((reference-origin (shape)
             (make-reference
              (find shape lazy-arrays :from-end t :key #'shape :test #'subshapep)
              shape identity)))
      (make-fusion (mapcar #'reference-origin shapes)))))

;; Create a fusion, assuming INPUTS are non-empty, non-overlapping lazy-arrays.
(defun make-fusion (inputs)
  (let ((shape (shape-union (mapcar #'shape inputs))))
    (trivia:match inputs
      ((list) (empty-array))
      ((list x) x)
      (_ (make-instance 'fusion
           :ntype (reduce #'petalisp.type-inference:ntype-union
                          inputs
                          :key #'ntype)
           :inputs inputs
           :shape shape)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Immediates

(defmethod coerce-to-lazy-array ((lazy-array lazy-array))
  lazy-array)

(defmethod coerce-to-lazy-array ((array array))
  (make-array-immediate array))

(defmethod coerce-to-lazy-array ((object t))
  (make-scalar-immediate object))

(defun make-scalar-immediate (object)
  (make-instance 'array-immediate
    :shape (~)
    :ntype (petalisp.type-inference:ntype-of object)
    :storage (petalisp.type-inference:make-rank-zero-array object)))

(defun make-array-immediate (array &optional reusablep)
  (check-type array array)
  (if (zerop (array-total-size array))
      (empty-array)
      (make-instance 'array-immediate
        :shape (shape array)
        :storage array
        :reusablep reusablep
        :ntype (petalisp.type-inference:array-element-ntype array))))

(defun make-range-immediate (range)
  (make-instance 'range-immediate
    :shape (make-shape (list range))
    :ntype
    (petalisp.type-inference:ntype-union
     (petalisp.type-inference:ntype-of (range-start range))
     (petalisp.type-inference:ntype-of (range-end range)))))

(defun indices (array-or-shape &optional (axis 0))
  (cond ((null array-or-shape)
         (empty-array))
        ((shapep array-or-shape)
         (let ((rank (shape-rank array-or-shape)))
           (unless (<= 0 axis (1- rank))
             (error "~@<Invalid axis ~A for a shape with rank ~D.~:@>" axis rank))
           (make-reference
            (make-range-immediate (nth axis (shape-ranges array-or-shape)))
            array-or-shape
            (make-transformation
             :input-rank rank
             :output-mask (vector axis)))))
        (t (indices (shape array-or-shape)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Applications

(declaim (inline α))
(defun α (function &rest arrays)
  (declare (dynamic-extent arrays))
  (multiple-value-bind (inputs shape)
      (broadcast-list-of-arrays arrays)
    (α-aux 1 shape (coerce function 'function) inputs)))

(declaim (inline α*))
(defun α* (n-values function &rest arrays)
  (declare (petalisp.type-inference:multiple-value-count n-values)
           (dynamic-extent arrays))
  (multiple-value-bind (inputs shape)
      (broadcast-list-of-arrays arrays)
    (α-aux n-values shape (coerce function 'function) inputs)))

(declaim (notinline α-aux))
(defun α-aux (n-outputs shape function inputs)
  (if (null shape)
      (empty-arrays n-outputs)
      (petalisp.type-inference:specialize
       function
       inputs
       #'ntype
       (lambda (constant)
         (reshape constant shape))
       (lambda (ntypes function inputs)
         (values-list
          (loop for ntype in ntypes
                for value-n from 0
                collect
                (make-instance 'application
                  :operator function
                  :value-n value-n
                  :inputs inputs
                  :shape shape
                  :ntype ntype))))
       (lambda ()
         (values-list
          (loop for value-n below n-outputs
                collect
                (make-instance 'application
                  :operator function
                  :value-n value-n
                  :inputs inputs
                  :shape shape
                  :ntype (petalisp.type-inference:ntype 't))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reductions

(defun β (function &rest arrays)
  (multiple-value-bind (inputs input-shape)
      (broadcast-list-of-arrays arrays)
    (if (or (null input-shape)
            (zerop (shape-rank input-shape)))
        (empty-arrays (length inputs))
        (let ((n-outputs (length inputs))
              (shape (shrink-shape input-shape)))
          (let* ((argument-ntypes (mapcar #'ntype inputs))
                 (ntypes
                   (multiple-value-list
                    (petalisp.type-inference:infer-ntypes
                     function
                     (append argument-ntypes argument-ntypes)
                     (lambda () (values)))))
                 (operator function))
            (labels ((next-ntype ()
                       (if (null ntypes)
                           (petalisp.type-inference:ntype 't)
                           (pop ntypes)))
                     (make-reduction (value-n)
                       (make-instance 'reduction
                         :operator operator
                         :value-n value-n
                         :inputs inputs
                         :shape shape
                         :ntype (next-ntype))))
              (values-list
               (loop for value-n below n-outputs
                     collect (make-reduction value-n)))))))))
