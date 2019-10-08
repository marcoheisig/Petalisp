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
  (let ((array-shape (shape lazy-array))
        (shape-size 1)
        (array-size 1)
        (similar-shapes-p t))
    (loop for shape-range in (shape-ranges shape)
          for array-range in (shape-ranges array-shape) do
            (let ((shape-range-size (range-size shape-range))
                  (array-range-size (range-size array-range)))
              (setf shape-size (* shape-size shape-range-size))
              (setf array-size (* array-size array-range-size))
              (unless (= shape-range-size array-range-size)
                (setf similar-shapes-p nil))))
    (if (and (= shape-size array-size)
             (not similar-shapes-p))
        ;; Case 1 - Reshaping while preserving the number of elements.
        (unflatten (flatten lazy-array) shape)
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

;; Turn an arbitrary lazy array into a rank one array.
(defun flatten (lazy-array)
  ;; TODO
  )

;; Turn a rank one array into an array of the supplied SHAPE.
(defun unflatten (lazy-array shape)
  (assert (= (rank lazy-array) 1))
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
    ;; TODO
    ))

;; Turn the range at the supplied AXIS with size N into a range of size K,
;; followed by a range of size N / K.
(defun insert-axis-before (lazy-array axis k)
  (let* ((shape (shape lazy-array))
         (rank (rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 axis))
         (suffix (subseq ranges (1+ axis)))
         (rest-size (/ (range-size (nth axis ranges)) k)))
    (flet ((select (index)
             (make-shape
              (append
               prefix
               (list (range (* index rest-size) (1- (* (1+ index) rest-size))))
               suffix)))
           (move (offset)
             (let ((output-mask (make-array (1+ rank)))
                   (offsets (make-array (1+ rank))))
               (loop for index below axis do
                 (setf (aref output-mask index) index)
                 (setf (aref offsets index) 0))
               (setf (aref output-mask axis) nil)
               (setf (aref offsets axis) offset)
               (setf (aref output-mask (1+ axis)) axis)
               (setf (aref offsets (1+ axis)) (- (* rest-size offset)))
               (loop for index from (+ axis 2) to rank do
                 (setf (aref output-mask index) (1- index))
                 (setf (aref offsets index) 0))
               (make-transformation
                :input-rank rank
                :output-mask output-mask
                :offsets offsets))))
      (apply #'fuse
             (loop for index below k
                   collect
                   (reshape lazy-array (select index) (move index)))))))

;; Turn the range at axis I with size N into a range of size N / K,
;; followed by a range of size K.
(defun insert-axis-after (lazy-array axis k)
  (let* ((shape (shape lazy-array))
         (rank (rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 axis))
         (suffix (subseq ranges (1+ axis)))
         (n (range-size (nth axis ranges))))
    (flet ((select (index)
             (make-shape
              (append
               prefix
               (list (range index k (1- n)))
               suffix)))
           (move (offset)
             (let ((output-mask (make-array (1+ rank)))
                   (offsets (make-array (1+ rank)))
                   (scalings (make-array (1+ rank))))
               (loop for index below axis do
                 (setf (aref output-mask index) index)
                 (setf (aref offsets index) 0)
                 (setf (aref scalings index) 1))
               (setf (aref output-mask axis) axis)
               (setf (aref offsets axis) (- (/ offset k)))
               (setf (aref scalings axis) (/ k))
               (setf (aref output-mask (1+ axis)) nil)
               (setf (aref offsets (1+ axis)) offset)
               (setf (aref scalings (1+ axis)) 1)
               (loop for index from (+ axis 2) to rank do
                 (setf (aref output-mask index) (1- index))
                 (setf (aref offsets index) 0)
                 (setf (aref scalings index) 0))
               (make-transformation
                :input-rank rank
                :output-mask output-mask
                :offsets offsets
                :scalings scalings))))
      (apply #'fuse
             (loop for index below k
                   collect
                   (reshape lazy-array (select index) (move index)))))))

;; Combine the range at the specified AXIS with the next one.
#+(or)
(defun combine-axes (lazy-array axis)
  (let* ((shape (shape lazy-array))
         (rank (shape-rank shape))
         (ranges (shape-ranges shape))
         (prefix (subseq ranges 0 axis))
         (suffix (subseq ranges (+ axis 2)))
         (range-1 (nth axis ranges))
         (range-2 (nth (1+ axis) ranges))
         (size-1 (range-size range-1))
         (size-2 (range-size range-2)))
    ;; There are two ways to combine the axes - by appending SIZE-1 arrays
    ;; entries, or by interleaving SIZE-2 arrays.  We pick the more
    ;; economic one.
    (if (< size-1 size-2)
        (flet ((select (index)
                 (make-shape
                  (append
                   prefix
                   (list (range index))
                   suffix)))
               (move (offset)
                 (let ((output-mask (make-array (1- rank))))
                   (loop for index below rank do
                     (setf (aref output-mask index) index))
                   (make-transformation
                    :input-rank rank
                    :output-mask output-mask))))
          (apply
           #'fuse
           (loop for index below size-1
                 collect
                 (reshape lazy-array (select index) (move index)))))
        (apply
         #'fuse
         (loop for index below size-2
               collect
               (reshape lazy-array (select index) (move index)))))))

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
