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
(defmethod make-reference
    ((reference reference) (shape shape) (transformation transformation))
  (make-reference
   (input reference)
   shape
   (compose-transformations
    (transformation reference)
    transformation)))

;;; Drop references with no effect.
(defmethod make-reference
    ((lazy-array lazy-array) (shape shape) (identity-transformation identity-transformation))
  (if (set-equal (shape lazy-array) shape)
      lazy-array
      (call-next-method)))

;;; Handle empty shapes.
(defmethod make-reference
    ((lazy-array lazy-array) (empty-set empty-set) (transformation transformation))
  (empty-array))

;;; The default - construct a new reference.
(defmethod make-reference
    ((lazy-array lazy-array) (shape shape) (transformation transformation))
  (make-instance 'reference
    :type-code (type-code lazy-array)
    :inputs (list lazy-array)
    :shape shape
    :transformation transformation))

;;; Error handling.
(defmethod make-reference :before
    ((lazy-array lazy-array) (shape shape) (transformation transformation))
  (let ((relevant-shape (transform shape transformation))
        (input-shape (shape lazy-array)))
    (unless (and (= (rank relevant-shape) (rank input-shape))
                 (set-subsetp relevant-shape input-shape))
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
             (assert (not (set-intersectionp shape-1 shape-2)) ()
                     "~@<The index shapes of the arguments to a fusion operation ~
                         must be disjoint, but shape ~S and shape ~S have the ~
                         common subshape ~S.~:@>"
                     shape-1
                     shape-2
                     (set-intersection shape-1 shape-2)))))
       lazy-arrays :length 2 :copy nil))
    (make-fusion lazy-arrays)))

(defun fuse* (&rest inputs)
  (let* ((lazy-arrays (sanitize-fusion-inputs inputs))
         (shapes (subdivision (mapcar #'shape lazy-arrays)))
         (identity (identity-transformation (rank (first lazy-arrays)))))
    (flet ((reference-origin (shape)
             (make-reference
              (find shape lazy-arrays :from-end t :key #'shape :test #'set-subsetp)
              shape identity)))
      (make-fusion (mapcar #'reference-origin shapes)))))

;; Create a fusion, assuming INPUTS are non-empty, non-overlapping lazy-arrays.
(defun make-fusion (inputs)
  (let ((shape (shape-union (mapcar #'shape inputs))))
    (trivia:match inputs
      ((list) (empty-array))
      ((list x) x)
      (_ (make-instance 'fusion
           :type-code (reduce #'petalisp.type-codes:type-code-union
                              inputs
                              :key #'type-code)
           :inputs inputs
           :shape shape)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Immediates

(defun make-array-immediate (array &optional reusablep)
  (check-type array array)
  (if (zerop (array-total-size array))
      (empty-array)
      (make-instance 'array-immediate
        :shape (shape array)
        :storage array
        :reusablep reusablep
        :type-code (petalisp.type-codes:array-element-type-code array))))

(defun make-range-immediate (range)
  (make-instance 'range-immediate
    :shape (make-shape (list range))
    :type-code
    (petalisp.type-codes:type-code-union
     (petalisp.type-codes:type-code-of (range-start range))
     (petalisp.type-codes:type-code-of (range-end range)))))

(defun indices (array-or-shape &optional (axis 0))
  (let ((shape (if (shapep array-or-shape)
                   array-or-shape
                   (shape array-or-shape))))
    (if (set-emptyp shape)
        (empty-array)
        (let ((rank (rank shape)))
          (unless (<= 0 axis (1- rank))
            (error "Invalid axis ~A for a shape with rank ~D." axis rank))
          (make-reference
           (make-range-immediate (nth axis (ranges shape)))
           shape
           (make-transformation
            :input-rank rank
            :output-mask (vector axis)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type Inference

(define-condition invalid-call (error)
  ((%function :initarg :function :reader invalid-call-function)
   (%argument-types :initarg :argument-types :reader invalid-call-argument-types)))

(defmethod print-object ((invalid-call invalid-call) stream)
  (format stream
          "~@<Invalid call to ~S with ~
           ~{~#[no arguments~;~
                one argument of type ~S~;~
                arguments of types ~a and ~a~:;~
                arguments of types ~@{~a~#[~;, and ~:;, ~]~}~
                ~]~:}.~:@>"
          (invalid-call-function invalid-call)
          (invalid-call-argument-types invalid-call)))


(defun infer-type-codes (function argument-type-codes)
  (let ((type-codes (multiple-value-list
                     (apply #'petalisp.type-codes:values-type-codes
                            function
                            argument-type-codes))))
    (unless (or (null type-codes)
                (not (petalisp.type-codes:empty-type-code-p (first type-codes))))
      (error 'invalid-call
             :function function
             :argument-types
             (mapcar #'petalisp.type-codes:type-specifier-from-type-code
                     argument-type-codes)))
    type-codes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Applications

(declaim (inline α))
(defun α (function &rest arrays)
  (declare (dynamic-extent arrays))
  (multiple-value-bind (inputs shape)
      (broadcast-list-of-arrays arrays)
    (α-aux 1 shape (coerce function 'function) inputs)))

(deftype multiple-value-count ()
  `(integer 0 ,multiple-values-limit))

(declaim (inline α*))
(defun α* (n-values function &rest arrays)
  (declare (multiple-value-count n-values)
           (dynamic-extent arrays))
  (multiple-value-bind (inputs shape)
      (broadcast-list-of-arrays arrays)
    (α-aux n-values shape (coerce function 'function) inputs)))

(declaim (notinline α-aux))
(defun α-aux (n-outputs shape function inputs)
  (if (set-emptyp shape)
      (empty-arrays n-outputs)
      (let* ((argument-type-codes (mapcar #'type-code inputs))
             (type-codes (infer-type-codes function argument-type-codes))
             (operator function))
        (labels ((next-type-code ()
                   (if (null type-codes)
                       (petalisp.type-codes:type-code-from-type-specifier 't)
                       (pop type-codes)))
                 (make-application (value-n)
                   (make-instance 'application
                     :operator operator
                     :value-n value-n
                     :inputs inputs
                     :shape shape
                     :type-code (next-type-code))))
          (case n-outputs
            (0 (values))
            (1 (values (make-application 0)))
            (otherwise
             (values-list
              (loop for value-n below n-outputs
                    collect (make-application value-n)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reductions

(defun β (function &rest arrays)
  (multiple-value-bind (inputs input-shape)
      (broadcast-list-of-arrays arrays)
    (let ((n-outputs (length inputs))
          (shape (shrink-shape input-shape)))
      (if (set-emptyp shape)
          (empty-arrays n-outputs)
          (let* ((argument-type-codes (mapcar #'type-code inputs))
                 (reduction-type-codes (append argument-type-codes argument-type-codes))
                 (type-codes (infer-type-codes function reduction-type-codes))
                 (operator function))
            (labels ((next-type-code ()
                       (if (null type-codes)
                           (petalisp.type-codes:type-code-from-type-specifier 't)
                           (pop type-codes)))
                     (make-reduction (value-n)
                       (make-instance 'reduction
                         :operator operator
                         :value-n value-n
                         :inputs inputs
                         :shape shape
                         :type-code (next-type-code))))
              (case n-outputs
                (0 (values))
                (1 (values (make-reduction 0)))
                (otherwise
                 (values-list
                  (loop for value-n below n-outputs
                        collect (make-reduction value-n)))))))))))
