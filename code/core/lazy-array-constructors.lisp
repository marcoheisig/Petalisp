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

(defun reshape (array &rest modifiers)
  (reduce (lambda (lazy-array modifier)
            (if (transformationp modifier)
                (reshape-using-transformation lazy-array modifier)
                (reshape-using-shape lazy-array (shape modifier))))
          modifiers
          :initial-value (coerce-to-lazy-array array)))

(defun reshape-using-transformation (lazy-array transformation)
  (make-reference
   lazy-array
   (transform (shape lazy-array) transformation)
   (invert-transformation transformation)))

(defun reshape-using-shape (lazy-array shape)
  (let ((array-shape (shape lazy-array)))
    (if (and (= (shape-size array-shape)
                (shape-size shape))
             ;; If the shapes are congruent, i.e., ranges of equal sizes
             ;; (ignoring one element ranges) occur in the same order, we
             ;; don't need the full reordering machinery.  Ordinary shape
             ;; transformations can deal with that.
             (not (shapes-congruent-p array-shape shape)))
        ;; Case 1 - Reshaping while preserving the number of elements.
        (reshape-using-equally-large-shape lazy-array shape)
        ;; Case 2 - Broadcasting or selection of a subspace.
        (multiple-value-bind (transformation broadcast-p select-p)
            (make-shape-transformation array-shape shape)
          ;; We do not allow transformations that broadcast some ranges, but
          ;; shrink others.  Otherwise, we would risk ambiguity with case 1.
          (unless (not (and broadcast-p select-p))
            (error "~@<Cannot broadcast the array ~S ~
                 to the shape ~S.~:@>"
                   lazy-array shape))
          (make-reference lazy-array shape transformation)))))

(defun shapes-congruent-p (shape-1 shape-2)
  (labels ((step-1/skip-left (left-ranges right-ranges)
             (if (and (not (null left-ranges))
                      (size-one-range-p (first left-ranges)))
                 (step-1/skip-left (rest left-ranges) right-ranges)
                 (step-2/skip-right left-ranges right-ranges)))
           (step-2/skip-right (left-ranges right-ranges)
             (if (and (not (null right-ranges))
                      (size-one-range-p (first right-ranges)))
                 (step-2/skip-right left-ranges (rest right-ranges))
                 (step-3/compare left-ranges right-ranges)))
           (step-3/compare (left-ranges right-ranges)
             (if (null left-ranges)
                 (null right-ranges)
                 (if (null right-ranges)
                     nil
                     (and (= (range-size (first left-ranges))
                             (range-size (first right-ranges)))
                          (step-1/skip-left (rest left-ranges)
                                            (rest right-ranges)))))))
    (step-1/skip-left (shape-ranges shape-1)
                      (shape-ranges shape-2))))

(defun reshape-using-equally-large-shape (lazy-array shape)
  (unflatten (flatten lazy-array) shape))

(defun flatten (lazy-array)
  (if (zerop (rank lazy-array))
      (make-reference lazy-array (~ 0) (τ (0) nil))
      (multiple-value-bind (vector-of-prime-factors pivot)
          (factorize-shape (shape lazy-array))
        (setf lazy-array (reshape lazy-array (collapsing-transformation (shape lazy-array))))
        ;; Flatten all ranges above PIVOT.
        (loop for index from (1+ pivot) below (length vector-of-prime-factors)
              for prime-factors = (aref vector-of-prime-factors index) do
                (loop for prime-factor in (rest prime-factors) do
                  (setf lazy-array (insert-axis-before lazy-array (1+ pivot) prime-factor))
                  (setf lazy-array (remove-axis-after lazy-array pivot)))
                (setf lazy-array (remove-axis-after lazy-array pivot)))
        ;; Flatten all ranges below PIVOT.
        (loop for index from (1- pivot) downto 0
              for prime-factors = (aref vector-of-prime-factors index) do
                (loop for prime-factor in (rest prime-factors) do
                  (setf lazy-array (insert-axis-after lazy-array index prime-factor))
                  (setf lazy-array (remove-axis-before lazy-array (1+ index))))
                (setf lazy-array (remove-axis-before lazy-array (1+ index))))
        lazy-array)))

(defun unflatten (lazy-array shape)
  (multiple-value-bind (vector-of-prime-factors pivot)
      (factorize-shape shape)
    ;; Unflatten all ranges above PIVOT.
    (loop for index from (1- (length vector-of-prime-factors)) above pivot
          for prime-factors = (aref vector-of-prime-factors index) do
            (setf lazy-array (insert-axis-after lazy-array 0 (first prime-factors)))
            (loop for prime-factor in (rest prime-factors) do
              (setf lazy-array (insert-axis-after lazy-array 0 prime-factor))
              (setf lazy-array (remove-axis-before lazy-array 2))))
    ;; Unflatten all ranges below PIVOT.
    (loop for index from 0 below pivot
          for prime-factors = (aref vector-of-prime-factors index) do
            (setf lazy-array (insert-axis-before lazy-array index (first prime-factors)))
            (loop for prime-factor in (rest prime-factors) do
              (setf lazy-array (insert-axis-before lazy-array (1+ index) prime-factor))
              (setf lazy-array (remove-axis-after lazy-array index))))
    (make-reference lazy-array shape (make-shape-transformation (shape lazy-array) shape))))

(defun factorize-shape (shape)
  (let ((vector-of-prime-factors (make-array (rank shape)))
        (most-positive-prime-factor 1)
        (most-positive-prime-factor-index 0))
    (loop for range in (shape-ranges shape)
          for index from 0 do
            (let* ((prime-factors (petalisp.utilities:prime-factors (range-size range)))
                   (max (apply #'max prime-factors)))
              (setf (aref vector-of-prime-factors index) prime-factors)
              (when (>= max most-positive-prime-factor)
                (setf most-positive-prime-factor max)
                (setf most-positive-prime-factor-index index))))
    (values vector-of-prime-factors most-positive-prime-factor-index)))

;; Turn the range at the supplied AXIS with size N into a range of size K,
;; followed by a range of size N / K.
(defun insert-axis-before (lazy-array axis k)
  (let* ((shape (shape lazy-array))
         (rank (rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 axis))
         (range (nth axis ranges))
         (suffix (subseq ranges (1+ axis)))
         (range-2 (range 0 (1- (/ (range-size range) k)))))
    (apply #'fuse
           (loop for offset below k
                 collect
                 (make-reference
                  lazy-array
                  (make-shape
                   (append prefix (list (range offset) range-2) suffix))
                  (let ((input-mask (make-array (1+ rank) :initial-element nil))
                        (output-mask (make-array rank :initial-element nil))
                        (offsets (make-array rank :initial-element 0)))
                    (loop for index below axis do
                      (setf (aref output-mask index) index))
                    (setf (aref input-mask axis) offset)
                    (setf (aref output-mask axis) (1+ axis))
                    (setf (aref offsets axis) (* offset (range-size range-2)))
                    (loop for index from (1+ axis) below rank do
                      (setf (aref output-mask index) (1+ index)))
                    (make-transformation
                     :input-mask input-mask
                     :output-mask output-mask
                     :offsets offsets)))))))

;; Turn the range at axis I with size N into a range of size N / K,
;; followed by a range of size K.
(defun insert-axis-after (lazy-array axis k)
  (let* ((shape (shape lazy-array))
         (rank (rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 axis))
         (suffix (subseq ranges (1+ axis)))
         (n (range-size (nth axis ranges)))
         (range-1 (range 0 (1- (/ n k)))))
    (apply #'fuse
           (loop for offset below k
                 collect
                 (make-reference
                  lazy-array
                  (make-shape
                   (append prefix (list range-1 (range offset)) suffix))
                  (let ((input-mask (make-array (1+ rank) :initial-element nil))
                        (output-mask (make-array rank :initial-element nil))
                        (offsets (make-array rank :initial-element 0))
                        (scalings (make-array rank :initial-element 1)))
                    (loop for index below axis do
                      (setf (aref output-mask index) index))
                    (setf (aref output-mask axis) axis)
                    (setf (aref offsets axis) offset)
                    (setf (aref scalings axis) k)
                    (setf (aref input-mask (1+ axis)) offset)
                    (loop for index from (1+ axis) below rank do
                      (setf (aref output-mask index) (1+ index)))
                    (make-transformation
                     :input-mask input-mask
                     :output-mask output-mask
                     :offsets offsets
                     :scalings scalings)))))))

(defun remove-axis-before (lazy-array axis)
  (let* ((shape (shape lazy-array))
         (rank (shape-rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 (1- axis)))
         (suffix (subseq ranges (1+ axis)))
         (range-1 (nth (1- axis) ranges))
         (range-2 (nth axis ranges))
         (size-1 (range-size range-1))
         (size-2 (range-size range-2)))
    (apply #'fuse
           (loop for offset below size-1
                 collect
                 (make-reference
                  lazy-array
                  (make-shape
                   (append prefix (list (range (* offset size-2) (1- (* (1+ offset) size-2)))) suffix))
                  (let ((input-mask (make-array (1- rank) :initial-element nil))
                        (output-mask (make-array rank :initial-element nil))
                        (offsets (make-array rank :initial-element 0)))
                    (loop for index below (1- axis) do
                      (setf (aref output-mask index) index))
                    (setf (aref output-mask (1- axis)) nil)
                    (setf (aref offsets (1- axis)) offset)
                    (setf (aref output-mask axis) (1- axis))
                    (setf (aref offsets axis) (- (* offset size-2)))
                    (loop for index from (1+ axis) below rank do
                      (setf (aref output-mask index) (1- index)))
                    (make-transformation
                     :input-mask input-mask
                     :output-mask output-mask
                     :offsets offsets)))))))

(defun remove-axis-after (lazy-array axis)
  (let* ((shape (shape lazy-array))
         (rank (shape-rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 axis))
         (suffix (subseq ranges (+ axis 2)))
         (range-1 (nth axis ranges))
         (range-2 (nth (1+ axis) ranges))
         (size-1 (range-size range-1))
         (size-2 (range-size range-2)))
    (apply #'fuse
           (loop for offset below size-2
                 collect
                 (make-reference
                  lazy-array
                  (make-shape
                   (append prefix (list (range offset size-2 (1- (* size-1 size-2)))) suffix))
                  (let ((input-mask (make-array (1- rank) :initial-element nil))
                        (output-mask (make-array rank :initial-element nil))
                        (scalings (make-array rank :initial-element 1))
                        (offsets (make-array rank :initial-element 0)))
                    (loop for index below axis do
                      (setf (aref output-mask index) index))
                    (setf (aref output-mask axis) axis)
                    (setf (aref offsets axis) (- (/ offset size-2)))
                    (setf (aref scalings axis) (/ size-2))
                    (setf (aref output-mask (1+ axis)) nil)
                    (setf (aref offsets (1+ axis)) offset)
                    (loop for index from (+ axis 2) below rank do
                      (setf (aref output-mask index) (1- index)))
                    (make-transformation
                     :input-mask input-mask
                     :output-mask output-mask
                     :offsets offsets
                     :scalings scalings)))))))

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
      (alexandria:map-combinations
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
             (let ((origin (find shape lazy-arrays :from-end t :key #'shape :test #'subshapep)))
               (assert origin)
               (make-reference origin shape identity))))
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
