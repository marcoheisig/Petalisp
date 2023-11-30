;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun make-identity-transformation (rank)
  (let ((input-mask (make-array rank :initial-element nil))
        (output-mask (make-array rank))
        (scalings (make-array rank :initial-element 1))
        (offsets (make-array rank :initial-element 0)))
    (loop for index below rank do
      (setf (svref output-mask index) index))
    (let ((result
            (%make-identity-transformation
             rank
             rank
             input-mask
             output-mask
             scalings
             offsets
             nil)))
      (setf (transformation-inverse result) result)
      result)))

(let* ((cache-size 100)
       (cache (make-array cache-size)))
  (loop for rank below cache-size do
    (setf (svref cache rank)
          (make-identity-transformation rank)))
  (defun identity-transformation (rank)
    (declare (rank rank))
    (if (< rank cache-size)
        (the transformation (aref cache rank))
        (make-identity-transformation rank))))

(defun make-transformation
    (&key
       (input-rank nil input-rank-supplied-p)
       (output-rank nil output-rank-supplied-p)
       (input-mask nil input-mask-supplied-p)
       (output-mask nil output-mask-supplied-p)
       (scalings nil scalings-supplied-p)
       (offsets nil offsets-supplied-p))
  ;; Attempt to derive the input and output rank.
  (multiple-value-bind (input-rank output-rank)
      (labels ((two-value-fixpoint (f x1 x2)
                 (multiple-value-bind (y1 y2) (funcall f x1 x2)
                   (if (and (eql x1 y1)
                            (eql x2 y2))
                       (values x1 x2)
                       (two-value-fixpoint f y1 y2))))
               (narrow-input-and-output-rank (i o)
                 (values
                  (cond (i i)
                        (input-rank-supplied-p input-rank)
                        (input-mask-supplied-p (length input-mask))
                        (o o))
                  (cond (o o)
                        (output-rank-supplied-p output-rank)
                        (output-mask-supplied-p (length output-mask))
                        (offsets-supplied-p (length offsets))
                        (scalings-supplied-p (length scalings))
                        (i i)))))
        (two-value-fixpoint #'narrow-input-and-output-rank nil nil))
    (check-type input-rank rank)
    (check-type output-rank rank)
    ;; Canonicalize all sequence arguments.
    (multiple-value-bind (input-mask identity-inputs-p)
        (canonicalize-inputs input-mask input-mask-supplied-p input-rank)
      (declare (simple-vector input-mask))
      (multiple-value-bind (output-mask scalings offsets identity-outputs-p)
          (canonicalize-outputs
           input-rank output-rank input-mask
           output-mask output-mask-supplied-p
           scalings scalings-supplied-p
           offsets offsets-supplied-p)
        (declare (simple-vector output-mask scalings offsets))
        (if (and (= input-rank output-rank) identity-inputs-p identity-outputs-p)
            (identity-transformation input-rank)
            (%make-hairy-transformation
             input-rank output-rank
             input-mask output-mask
             scalings offsets
             ;; A transformation is invertible, if each unused argument
             ;; has a corresponding input constraint.
             (loop for constraint across input-mask
                   for input-index from 0
                   always (or constraint (find input-index output-mask)))))))))

(defun canonicalize-inputs (input-mask supplied-p input-rank)
  (if (not supplied-p)
      (values (make-array input-rank :initial-element nil) t)
      (let ((vector (coerce input-mask 'simple-vector))
            (identity-p t))
        (unless (= (length vector) input-rank)
          (error "~@<The input mask ~S does not match the input rank ~S.~:@>"
                 vector input-rank))
        (loop for element across vector do
          (typecase element
            (null)
            (integer (setf identity-p nil))
            (otherwise
             (error "~@<The object ~S is not a valid input mask element.~:@>"
                    element))))
        (values vector identity-p))))

(defun canonicalize-outputs (input-rank output-rank input-mask
                             output-mask output-mask-supplied-p
                             scalings scalings-supplied-p
                             offsets offsets-supplied-p)
  (declare (rank input-rank output-rank))
  (let ((output-mask (if (not output-mask-supplied-p)
                         (let ((vector (make-array output-rank :initial-element nil)))
                           (dotimes (index (min input-rank output-rank) vector)
                             (setf (svref vector index) index)))
                         (coerce output-mask 'simple-vector)))
        (scalings (if (not scalings-supplied-p)
                      (make-array output-rank :initial-element 1)
                      (coerce scalings 'simple-vector)))
        (offsets (if (not offsets-supplied-p)
                     (make-array output-rank :initial-element 0)
                     (coerce offsets 'simple-vector))))
    (unless (= (length output-mask) output-rank)
      (error "~@<The output mask ~S does not match the output rank ~S.~:@>"
             output-mask output-rank))
    (unless (= (length scalings) output-rank)
      (error "~@<The scaling vector ~S does not match the output rank ~S.~:@>"
             scalings output-rank))
    (unless (= (length offsets) output-rank)
      (error "~@<The offset vector ~S does not match the output rank ~S.~:@>"
             offsets output-rank))
    (let (;; The IDENTITY-P flag is set to NIL as soon as an entry is
          ;; detected that deviates from the identity values.
          (identity-p t)
          ;; We use a bitmask to track which input indices have already
          ;; occurred in the output mask.
          (bitmask 0))
      (loop for output-index from 0
            for input-index across output-mask
            for scaling across scalings
            for offset across offsets do
              (unless (rationalp scaling)
                (error "~@<The scaling vector element ~S is not a rational.~:@>"
                       scaling))
              (unless (rationalp offset)
                (error "~@<The offset vector element ~S is not a rational.~:@>"
                       offset))
              (typecase input-index
                ;; Case 1 - The output mask entry is NIL, so all we have to
                ;; ensure is that the corresponding scaling value is zero.
                (null
                 (setf (aref scalings output-index) 0)
                 (setf identity-p nil))
                ((not integer)
                 (error "~@<The object ~S is not a valid output mask element.~:@>"
                        input-index))
                (integer
                 (unless (array-in-bounds-p input-mask input-index)
                   (error "~@<The output mask element ~S is not below the input rank ~S.~:@>"
                          input-index input-rank))
                 (let ((bit (ash 1 input-index)))
                   (unless (zerop (logand bit bitmask))
                     (error "~@<The output mask ~S contains duplicate entries.~:@>"
                            output-mask))
                   (setf bitmask (logior bit bitmask)))
                 (let ((input-constraint (aref input-mask input-index)))
                   (etypecase input-constraint
                     ;; Case 2 - The output mask entry is non-NIL, but
                     ;; references an input with an input constraint.  In
                     ;; this case, we need to update the offset such that
                     ;; we can set the output mask entry to NIL and the
                     ;; scaling to zero.
                     (integer
                      (setf (aref offsets output-index)
                            (+ (* scaling input-constraint) offset))
                      (setf (aref scalings output-index) 0)
                      (setf (aref output-mask output-index) nil)
                      (setf identity-p nil))
                     (null
                      (cond ((zerop scaling)
                             ;; Case 3 - The output mask entry is non-NIL and
                             ;; references an unconstrained input, but the scaling
                             ;; is zero.
                             (setf (aref output-mask output-index) nil)
                             (setf identity-p nil))
                            ;; Case 4 - We are dealing with a
                            ;; transformation that is not the identity.
                            ((or (/= input-index output-index)
                                 (/= 1 scaling)
                                 (/= 0 offset))
                             (setf identity-p nil))
                            ;; Case 5 - We have to do nothing.
                            (t (values)))))))))
      (values output-mask scalings offsets identity-p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary Constructors

(defun collapsing-transformation (shape)
  (invert-transformation
   (from-storage-transformation shape)))

;;; Returns a non-permuting, affine transformation from a zero based array
;;; with step size one to the given SHAPE.
(defun from-storage-transformation (shape)
  (if (loop for range in (shape-ranges shape)
            always
            (or (range-emptyp range)
                (and (= 0 (range-start range))
                     (= 1 (range-step range)))))
      (identity-transformation (shape-rank shape))
      (let* ((rank (shape-rank shape))
             (ranges (shape-ranges shape))
             (input-mask (make-array rank))
             (output-mask (make-array rank))
             (scalings (make-array rank))
             (offsets (make-array rank)))
        (loop for range in ranges
              for index from 0 do
                (cond ((range-emptyp range)
                       (setf (aref input-mask index) nil)
                       (setf (aref output-mask index) index)
                       (setf (aref scalings index) 0)
                       (setf (aref offsets index) 0))
                      ((range-with-size-one-p range)
                       (let ((value (range-start range)))
                         (setf (aref input-mask index) 0)
                         (setf (aref output-mask index) nil)
                         (setf (aref scalings index) 0)
                         (setf (aref offsets index) value)))
                      (t
                       (setf (aref input-mask index) nil)
                       (setf (aref output-mask index) index)
                       (setf (aref scalings index) (range-step range))
                       (setf (aref offsets index) (range-start range)))))
        (%make-hairy-transformation rank rank input-mask output-mask scalings offsets t))))

;;; Returns an invertible transformation that eliminates all ranges with
;;; size one from a supplied SHAPE.
(defun size-one-range-removing-transformation (shape)
  (let* ((rank (shape-rank shape))
         (ranges (shape-ranges shape))
         (size-one-ranges
           (loop for range in ranges
                 count (range-with-size-one-p range))))
    (if (zerop size-one-ranges)
        (identity-transformation rank)
        (let* ((input-rank rank)
               (output-rank (- rank size-one-ranges))
               (input-mask (make-array input-rank :initial-element nil))
               (output-mask (make-array  output-rank :initial-element nil))
               (scalings (make-array output-rank :initial-element 1))
               (offsets (make-array output-rank :initial-element 0))
               (output-index 0))
          (loop for range in ranges
                for input-index from 0 do
                  (if (range-with-size-one-p range)
                      (setf (aref input-mask input-index) (range-start range))
                      (progn
                        (setf (aref output-mask output-index) input-index)
                        (incf output-index))))
          (make-transformation
           :input-rank input-rank
           :output-rank output-rank
           :input-mask input-mask
           :output-mask output-mask
           :scalings scalings
           :offsets offsets)))))

(defun normalizing-transformation (shape)
  (let* ((f (size-one-range-removing-transformation shape))
         (s (transform-shape shape f))
         (g (collapsing-transformation s)))
    (compose-transformations g f)))
