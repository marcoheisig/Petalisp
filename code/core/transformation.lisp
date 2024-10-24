;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;; Petalisp transformations are a combination of the following five
;;; elementary operations:
;;;
;;; (1) translating the indices by a constant
;;; (2) multiplying the indices by a constant
;;; (3) permuting the indices
;;; (4) introducing ranks with a one element range
;;; (5) removing ranks

(defstruct (transformation
            (:predicate transformationp)
            (:copier nil)
            (:constructor nil))
  (input-rank nil :type rank :read-only t)
  (output-rank nil :type rank :read-only t)
  ;; The input mask is a simple vector with as many entries as the input
  ;; rank of the transformation.  Each element is either an integer,
  ;; meaning the corresponding input must be of this value, or NIL, meaning
  ;; the input can be of any value.
  (input-mask nil :type simple-vector :read-only t)
  ;; The output mask is a simple vector with as many entries as the output
  ;; rank of the transformation.  Each element is either an integer N,
  ;; meaning the corresponding output is a function of the Nth input, or
  ;; NIL, meaning the output is a constant.
  ;;
  ;; To make the representation of transformations unambiguous, an output
  ;; mask entry must never reference an input that has an input constraint.
  (output-mask nil :type simple-vector :read-only t)
  ;; The scalings are a simple vector with as many entries as the output rank
  ;; of the transformation.  Each entry is a rational number, representing how
  ;; the input denoted by the corresponding entry in the output mask is to be
  ;; scaled.
  ;;
  ;; To make the representation of transformations unambiguous, the scaling
  ;; value must be zero if the corresponding output mask entry is NIL.
  (scalings nil :type simple-vector :read-only t)
  ;; The offsets are a simple vector with as many entries as the output rank of
  ;; the transformation.  Each entry is a rational number, representing how the
  ;; input denoted by the corresponding entry in the output mask is to be
  ;; shifted after scaling it.
  (offsets nil :type simple-vector :read-only t)
  ;; The INVERSE slot is used both to track whether a transformation is
  ;; invertible, and to cache that inverse.
  (inverse nil :type (or boolean transformation) :read-only nil))

(defstruct (identity-transformation
            (:include transformation)
            (:predicate identity-transformation-p)
            (:copier nil)
            (:constructor %make-identity-transformation
              (input-rank output-rank input-mask output-mask scalings offsets inverse))))

(defstruct (hairy-transformation
            (:include transformation)
            (:predicate hairy-transformation-p)
            (:copier nil)
            (:constructor %make-hairy-transformation
                (input-rank output-rank input-mask output-mask scalings offsets inverse))))

;; Forward declaration of the primary transformation constructors, because
;; they will be referenced before being defined.
(declaim (ftype (function (&key (:input-rank (or rank null))
                                (:output-rank (or rank null))
                                (:input-mask (or sequence null))
                                (:output-mask (or sequence null))
                                (:offsets (or sequence null))
                                (:scalings (or sequence null))))
                make-transformation)
         (ftype (function (rank))
                identity-transformation))

(defun transformation-identityp (transformation)
  (declare (transformation transformation))
  (identity-transformation-p transformation))

(defun transformation-invertiblep (transformation)
  (declare (transformation transformation))
  (or (transformation-identityp transformation)
      (and (transformation-inverse transformation) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric transformation= (transformation1 transformation2))

(defgeneric transformation-similar (transformation1 transformation2 delta))

(defgeneric compose-two-transformations (g f))

(defgeneric invert-transformation (transformation))

(defgeneric map-transformation-inputs (function transformation &key from-end))

(defgeneric map-transformation-outputs (function transformation &key from-end))

(defgeneric enlarge-transformation (transformation scale offset))

(defgeneric inflate-transformation (transformation n))

(defgeneric transform-index (index transformation)
  (:argument-precedence-order transformation index))

(defgeneric transform-shape (shape transformation)
  (:argument-precedence-order transformation shape))

(defgeneric transform-axis (axis transformation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TRANSFORMATION=

(defmethod transformation= ((t1 transformation)
                            (t2 transformation))
  nil)

(defmethod transformation= ((t1 identity-transformation)
                            (t2 identity-transformation))
  (= (transformation-input-rank t1)
     (transformation-input-rank t2)))

(defmethod transformation= ((t1 hairy-transformation)
                            (t2 hairy-transformation))
  (and (= (transformation-input-rank t1)
          (transformation-input-rank t2))
       (= (transformation-output-rank t1)
          (transformation-output-rank t2))
       (equalp (transformation-input-mask t1)
               (transformation-input-mask t2))
       (equalp (transformation-output-mask t1)
               (transformation-output-mask t2))
       (equalp (transformation-offsets t1)
               (transformation-offsets t2))
       (equalp (transformation-scalings t1)
               (transformation-scalings t2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TRANSFORMATION-SIMILAR

(defmethod transformation-similar ((t1 transformation)
                                   (t2 transformation)
                                   (delta integer))
  (and (= (transformation-input-rank t1)
          (transformation-input-rank t2))
       (= (transformation-output-rank t1)
          (transformation-output-rank t2))
       (equalp (transformation-input-mask t1)
               (transformation-input-mask t2))
       (equalp (transformation-output-mask t1)
               (transformation-output-mask t2))
       (equalp (transformation-scalings t1)
               (transformation-scalings t2))
       (loop for o1 across (transformation-offsets t1)
             for o2 across (transformation-offsets t2)
             always (<= (abs (- o1 o2)) delta))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPOSE-TWO-TRANSFORMATIONS

(defmethod compose-two-transformations :before ((g transformation)
                                                (f transformation))
  (unless (= (transformation-output-rank f)
             (transformation-input-rank g))
    (error "~@<Cannot compose the transformation ~S with input rank ~D
               with the transformation ~S with output rank ~D.~:@>"
           g (transformation-input-rank g)
           f (transformation-output-rank f))))

(defmethod compose-two-transformations
    ((g identity-transformation) (f transformation))
  f)

(defmethod compose-two-transformations
    ((g transformation) (f identity-transformation))
  g)

(defmethod compose-two-transformations
    ((g hairy-transformation) (f hairy-transformation))
  ;; A2 (A1 x + b1) + b2 = A2 A1 x + A2 b1 + b2
  (let ((input-rank (transformation-input-rank f))
        (output-rank (transformation-output-rank g)))
    (let ((f-output-mask (transformation-output-mask f))
          (f-offsets (transformation-offsets f))
          (f-scalings (transformation-scalings f))
          (input-mask (alexandria:copy-array (transformation-input-mask f)))
          (output-mask (make-array output-rank :initial-element nil))
          (scalings (make-array output-rank :initial-element 0))
          (offsets (make-array output-rank :initial-element 0)))
      (map-transformation-inputs
       (lambda (input-index g-constraint output-index)
           (declare (ignore output-index))
         (when (integerp g-constraint)
           (let ((f-mask (svref f-output-mask input-index))
                 (f-offset (svref f-offsets input-index))
                 (f-scaling (svref f-scalings input-index)))
             (if (null f-mask)
                 (unless (= f-offset g-constraint)
                   (error "~@<The output ~S of the transformation ~S ~
                              violates the input constraint of the transformation ~S.~:@>"
                          input-index f g))
                 (let ((old-constraint (svref input-mask f-mask))
                       (new-constraint (/ (- g-constraint f-offset) f-scaling)))
                   (unless (integerp new-constraint)
                     (error "~@<There is no valid integer suitable as argument ~S ~
                             to the transformation ~S such that it composes ~
                             with the transformation ~S.~:@>"
                            f-mask f g))
                   (unless (or (null old-constraint)
                               (= old-constraint new-constraint))
                     (error "~@<Composing the transformations ~S and ~S ~
                             yields the conflicting input constraints ~
                             ~S and ~S.~:@>"
                            g f new-constraint old-constraint))
                   (setf (svref input-mask f-mask) new-constraint))))))
       g)
      (map-transformation-outputs
       (lambda (output-index input-index a b)
         (etypecase input-index
           (null
            (setf (svref scalings output-index) 0)
            (setf (svref offsets output-index) b))
           (integer
            (let ((f-mask (svref f-output-mask input-index))
                  (f-scaling (svref f-scalings input-index))
                  (f-offset (svref f-offsets input-index)))
              (setf (svref output-mask output-index) f-mask)
              (setf (svref scalings output-index) (* a f-scaling))
              (setf (svref offsets output-index) (+ (* a f-offset) b))))))
       g)
      (if (and (= input-rank output-rank)
               (every #'null input-mask)
               (loop for index from 0
                     for input across output-mask
                     always (eql index input))
               (every (lambda (x) (eql x 1)) scalings)
               (every (lambda (x) (eql x 0)) offsets))
          (identity-transformation input-rank)
          (%make-hairy-transformation
           input-rank output-rank
           input-mask output-mask
           scalings offsets
           (and (transformation-inverse g)
                (transformation-inverse f)
                t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INVERT-TRANSFORMATION

(defmethod invert-transformation ((transformation identity-transformation))
  transformation)

(defmethod invert-transformation ((transformation hairy-transformation))
  (let ((inverse (transformation-inverse transformation)))
    (unless inverse
      (error "~@<The transformation ~S is not invertible.~:@>"
             transformation))
    (if (transformationp inverse)
        inverse
        ;;    f(x) = (Ax + b)
        ;; f^-1(x) = A^-1(x - b) = A^-1 x - A^-1 b
        (let* ((output-rank (transformation-input-rank transformation))
               (input-rank (transformation-output-rank transformation))
               (input-mask (make-array input-rank :initial-element nil))
               (output-mask (make-array output-rank :initial-element nil))
               (scalings (make-array output-rank :initial-element 0))
               (offsets (alexandria:copy-array (transformation-input-mask transformation))))
          (map-transformation-outputs
           (lambda (output-index input-index a b)
             (if (not input-index)
                 (progn (setf (aref input-mask output-index) b))
                 (progn (setf (aref output-mask input-index) output-index)
                        (setf (aref scalings input-index) (/ a))
                        (setf (aref offsets input-index) (- (/ b a))))))
           transformation)
          (setf (transformation-inverse transformation)
                (%make-hairy-transformation
                 input-rank output-rank
                 input-mask output-mask
                 scalings offsets
                 transformation))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MAP-TRANSFORMATION-INPUTS

(defmethod map-transformation-inputs ((function function)
                                      (transformation identity-transformation)
                                      &key from-end)
  (if (not from-end)
      (loop for input-index below (transformation-input-rank transformation) do
        (funcall function input-index nil input-index))
      (loop for input-index downfrom (1- (transformation-input-rank transformation)) to 0 do
        (funcall function input-index nil input-index))))

(defmethod map-transformation-inputs ((function function)
                                      (transformation hairy-transformation)
                                      &key from-end)
  (let ((input-mask (transformation-input-mask transformation))
        (output-mask (transformation-output-mask transformation)))
    (if (not from-end)
        (loop for input-index below (transformation-input-rank transformation) do
          (funcall function input-index
                   (aref input-mask input-index)
                   (position input-index output-mask)))
        (loop for input-index downfrom (1- (transformation-input-rank transformation)) to 0 do
          (funcall function input-index
                   (aref input-mask input-index)
                   (position input-index output-mask))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MAP-TRANSFORMATION-OUTPUTS

(defmethod map-transformation-outputs ((function function)
                                       (transformation identity-transformation)
                                       &key from-end)
  (if (not from-end)
      (loop for index below (transformation-output-rank transformation) do
        (funcall function index index 1 0))
      (loop for index downfrom (1- (transformation-output-rank transformation)) to 0 do
        (funcall function index index 1 0))))

(defmethod map-transformation-outputs
    ((function function)
     (transformation hairy-transformation)
     &key from-end)
  (let ((output-rank (transformation-output-rank transformation))
        (output-mask (transformation-output-mask transformation))
        (offsets (transformation-offsets transformation))
        (scalings (transformation-scalings transformation)))
    (flet ((process (output-index)
             (let ((input-index (aref output-mask output-index))
                   (scalings (aref scalings output-index))
                   (offset (aref offsets output-index)))
               (funcall function output-index input-index scalings offset))))
      (if (not from-end)
          (loop for output-index below output-rank do
            (process output-index))
          (loop for output-index downfrom (1- output-rank) to 0 do
            (process output-index))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENLARGE-TRANSFORMATION

(defmethod enlarge-transformation ((transformation identity-transformation)
                                   (scale (eql 1))
                                   (offset (eql 0)))
  (identity-transformation (1+ (transformation-input-rank transformation))))

(defmethod enlarge-transformation ((transformation identity-transformation)
                                   (scale rational)
                                   (offset rational))
  (let* ((rank (1+ (transformation-input-rank transformation)))
         (offsets (make-array rank :initial-element 0))
         (scalings (make-array rank :initial-element 1)))
    (setf (aref offsets 0) offset)
    (setf (aref scalings 0) scale)
    (make-transformation
     :input-rank rank
     :output-rank rank
     :scalings scalings
     :offsets offsets)))

(defmethod enlarge-transformation ((transformation hairy-transformation)
                                   (scaling rational)
                                   (offset rational))
  (let ((input-rank (1+ (transformation-input-rank transformation)))
        (output-rank (1+ (transformation-output-rank transformation))))
    (let ((input-mask (make-array input-rank :initial-element nil))
          (output-mask (make-array output-rank :initial-element (1- input-rank)))
          (scalings (make-array output-rank :initial-element scaling))
          (offsets (make-array output-rank :initial-element offset)))
      (replace input-mask (transformation-input-mask transformation) :start1 1)
      (replace output-mask (transformation-output-mask transformation) :start1 1)
      (replace scalings (transformation-scalings transformation) :start1 1)
      (replace offsets (transformation-offsets transformation) :start1 1)
      (setf (aref input-mask 0) nil)
      (%make-hairy-transformation
       input-rank output-rank
       input-mask output-mask
       scalings offsets
       (and (transformation-inverse transformation) t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INFLATE-TRANSFORMATION

(defmethod inflate-transformation :around
    ((transformation transformation)
     (n integer))
  (check-type n unsigned-byte "a non-negative integer")
  (if (zerop n)
      transformation
      (call-next-method)))

(defmethod inflate-transformation
    ((transformation identity-transformation)
     (n integer))
  (identity-transformation
   (+ (transformation-input-rank transformation) n)))

(defmethod inflate-transformation
    ((transformation hairy-transformation)
     (n integer))
  (let ((input-rank (+ (transformation-input-rank transformation) n))
        (output-rank (+ (transformation-output-rank transformation) n)))
    (let ((input-mask (make-array input-rank :initial-element nil))
          (output-mask (make-array output-rank :initial-element (1- input-rank)))
          (scalings (make-array output-rank :initial-element 1))
          (offsets (make-array output-rank :initial-element 0)))
      (replace input-mask (transformation-input-mask transformation))
      (replace output-mask (transformation-output-mask transformation))
      (replace scalings (transformation-scalings transformation))
      (replace offsets (transformation-offsets transformation))
      (dotimes (i n)
        (setf (aref output-mask (- output-rank 1 i))
              (- input-rank 1 i)))
      (%make-hairy-transformation
       input-rank output-rank
       input-mask output-mask
       scalings offsets
       (and (transformation-inverse transformation) t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TRANSFORM-INDEX

(defmethod transform-index :before
    ((sequence sequence)
     (transformation transformation))
  (unless (= (length sequence) (transformation-input-rank transformation))
    (error "~@<Cannot transform the sequence ~S of length ~D ~
               with the transformation ~S of input rank ~S.~:@>"
           sequence (length sequence)
           transformation (transformation-input-rank transformation))))

(defmethod transform-index :before
    ((sequence sequence)
     (transformation hairy-transformation))
  (map nil (lambda (constraint element)
             (unless (or (null constraint)
                         (not (numberp element))
                         (= constraint element))
               (error "~@<The number ~S violates the input constraint ~S ~
                          of the transformation ~S.~:@>"
                      element constraint transformation)))
       (transformation-input-mask transformation)
       sequence))

(defmethod transform-index
    ((sequence sequence)
     (transformation identity-transformation))
  sequence)

(defmethod transform-index
    ((list list)
     (transformation hairy-transformation))
  (let ((result '()))
    (flet ((push-output-expression (output-index input-index a b)
             (declare (ignore output-index))
             (let* ((x (if (not input-index)
                           0
                           (elt list input-index)))
                    (a*x (cond ((numberp x) (* a x))
                               ((= 1 a) x)
                               ((and) `(* ,a ,x))))
                    (a*x+b (cond ((numberp a*x) (+ a*x b))
                                 ((= b 0) a*x)
                                 ((= b  1) `(1+ ,a*x))
                                 ((= b -1) `(1- ,a*x))
                                 ((and) `(+ ,a*x ,b)))))
               (push A*x+b result))))
      (map-transformation-outputs #'push-output-expression transformation)
      (nreverse result))))

(defmethod transform-index
    ((vector vector)
     (transformation hairy-transformation))
  (let* ((output-rank (transformation-output-rank transformation))
         (result (make-array output-rank :element-type (array-element-type vector))))
    (map-transformation-outputs
     (lambda (output-index input-index a b)
       (setf (aref result output-index)
             (let* ((x (if (not input-index) 0 (aref vector input-index)))
                    (a*x (cond ((numberp x) (* a x))
                               ((= 1 a) x)
                               ((and) `(* ,a ,x)))))
               (cond ((numberp a*x) (+ a*x b))
                     ((= b 0) a*x)
                     ((= b  1) `(1+ ,a*x))
                     ((= b -1) `(1- ,a*x))
                     ((and) `(+ ,a*x ,b))))))
     transformation)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TRANSFORM-SHAPE

(defmethod transform-shape :before
    ((shape shape)
     (transformation transformation))
  (assert (= (shape-rank shape)
             (transformation-input-rank transformation)) ()
             "~@<Cannot apply the transformation ~A with input rank ~R ~
              to the index shape ~A with rank ~R.~:@>"
             transformation (transformation-input-rank transformation)
             shape (shape-rank shape)))

(defmethod transform-shape :before
    ((shape shape)
     (transformation hairy-transformation))
  (let ((input-mask (transformation-input-mask transformation)))
    (loop for range in (shape-ranges shape)
          for constraint across input-mask
          for index from 0 do
            (when constraint
              (unless (or (empty-range-p range)
                          (and (range-with-size-one-p range)
                               (= constraint (range-start range))))
                (error "~@<The ~:R axis of the shape ~W violates ~
                           the input constraint ~W of the transformation ~W.~:@>"
                       (1+ index) shape constraint transformation))))))

(defmethod transform-shape
    ((shape shape)
     (operator identity-transformation))
  shape)

(defmethod transform-shape
    ((shape shape)
     (transformation hairy-transformation))
  (let ((output-ranges (make-list (transformation-output-rank transformation)))
        (input-ranges (shape-ranges shape)))
    (map-transformation-outputs
     (lambda (output-index input-index scaling offset)
       (setf (elt output-ranges output-index)
             (if (not input-index)
                 (range offset (1+ offset))
                 (let* ((input-range (elt input-ranges input-index)))
                   (symbol-macrolet ((input-start (range-start input-range))
                                     (input-last (range-last input-range))
                                     (input-step (range-step input-range)))
                     (cond ((empty-range-p input-range)
                            (empty-range))
                           ((range-with-size-one-p input-range)
                            (range (+ (* scaling input-start) offset)
                                   (+ (* scaling input-start) offset 1)))
                           ((plusp scaling)
                            (range
                             (+ (* scaling input-start) offset)
                             (+ (* scaling input-last) offset 1)
                             (* scaling input-step)))
                           ((minusp scaling)
                            (range (+ (* scaling input-last) offset)
                                   (+ (* scaling input-start) offset 1)
                                   (- (* scaling input-step))))))))))
     transformation)
    (make-shape output-ranges)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TRANSFORM-AXIS

(defmethod transform-axis ((axis integer)
                           (transformation transformation))
  (unless (< -1 axis (transformation-input-rank transformation))
    (error "~@<The integer ~D is not a valid axis for the transformation ~S.~:@>"
           axis transformation)))

(defmethod transform-axis ((axis integer)
                           (transformation identity-transformation))
  axis)

(defmethod transform-axis ((axis integer)
                           (hairy-transformation hairy-transformation))
  (position axis (transformation-output-mask hairy-transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPOSE-TRANSFORMATIONS

(defun compose-transformations (transformation &rest more-transformations)
  (if (null more-transformations)
      (the transformation transformation)
      (reduce #'compose-two-transformations more-transformations
              :initial-value transformation)))

(define-compiler-macro compose-transformations (&whole whole &rest transformations)
  (trivia:match transformations
    ((list) whole)
    ((list transformation) (the transformation transformation))
    (_ (reduce (lambda (a b) `(compose-two-transformations ,a ,b)) transformations))))
