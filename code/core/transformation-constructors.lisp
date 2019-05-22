;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun make-transformation
    (&key
       (input-mask nil input-mask-supplied-p)
       (output-mask nil output-mask-supplied-p)
       (offsets nil offsets-supplied-p)
       (scalings nil scalings-supplied-p)
       (input-rank nil input-rank-supplied-p)
       (output-rank nil output-rank-supplied-p))
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
    (check-type input-rank array-rank)
    (check-type output-rank array-rank)
    ;; Canonicalize all sequence arguments.
    (multiple-value-bind (input-mask identity-input-mask-p)
        (canonicalize-input-mask input-mask input-mask-supplied-p input-rank)
      (declare (simple-vector input-mask)
               (boolean identity-input-mask-p))
      (multiple-value-bind (output-mask identity-output-mask-p)
          (canonicalize-output-mask output-mask output-mask-supplied-p output-rank input-rank)
        (declare (simple-vector output-mask)
                 (boolean identity-output-mask-p))
        (multiple-value-bind (scalings identity-scalings-p)
            (canonicalize-scalings scalings scalings-supplied-p output-rank)
          (declare (simple-vector scalings)
                   (boolean identity-scalings-p))
          (multiple-value-bind (offsets identity-offsets-p)
              (canonicalize-offsets offsets offsets-supplied-p output-rank)
            (declare (simple-vector offsets)
                     (boolean identity-offsets-p))
            (unless identity-input-mask-p
              (loop for input-index across output-mask
                    for scaling across scalings
                    do (assert (or (zerop scaling) input-index))))
            (if (and (= input-rank output-rank)
                     identity-input-mask-p
                     identity-output-mask-p
                     identity-scalings-p
                     identity-offsets-p)
                (identity-transformation input-rank)
                ;; A transformation is invertible, if each unused argument
                ;; has a corresponding input constraint.
                (if (loop for constraint across input-mask
                          for input-index from 0
                          always (or constraint (find input-index output-mask)))
                    (make-instance 'hairy-invertible-transformation
                      :input-rank input-rank
                      :output-rank output-rank
                      :input-mask input-mask
                      :output-mask output-mask
                      :scalings scalings
                      :offsets offsets)
                    (make-instance 'hairy-transformation
                      :input-rank input-rank
                      :output-rank output-rank
                      :input-mask input-mask
                      :output-mask output-mask
                      :scalings scalings
                      :offsets offsets)))))))))

(defun canonicalize-input-mask (value supplied-p input-rank)
  (if (not supplied-p)
      (values (make-sequence 'simple-vector input-rank :initial-element nil) t)
      (let ((vector (coerce value 'simple-vector))
            (identity-p t))
        (assert (= (length vector) input-rank))
        (loop for element across vector
              do (assert (typep element '(or rational null)))
              unless (eql element 0) do
                (setf identity-p nil))
        (values vector identity-p))))

(defun canonicalize-output-mask (value supplied-p output-rank input-rank)
  (if (not supplied-p)
      (let ((vector (make-sequence 'simple-vector output-rank :initial-element nil)))
        (loop for index below (min input-rank output-rank) do
          (setf (svref vector index) index))
        (values vector (= input-rank output-rank)))
      (let ((vector (coerce value 'simple-vector))
            (identity-p t))
        (assert (= (length vector) output-rank))
        (loop for index below output-rank
              for element across vector
              do (assert (typep element '(or array-rank null)))
              unless (eql element index) do
                (setf identity-p nil))
        (values vector identity-p))))

(defun canonicalize-scalings (value supplied-p output-rank)
  (if (not supplied-p)
      (values (make-sequence 'simple-vector output-rank :initial-element 1))
      (let ((vector (coerce value 'simple-vector))
            (identity-p t))
        (assert (= (length vector) output-rank))
        (loop for element across vector
              do (assert (rationalp element))
              unless (eql element 1) do
                (setf identity-p nil))
        (values vector identity-p))))

(defun canonicalize-offsets (value supplied-p output-rank)
  (if (not supplied-p)
      (values (make-sequence 'simple-vector output-rank :initial-element 0) t)
      (let ((vector (coerce value 'simple-vector))
            (identity-p t))
        (assert (= (length vector) output-rank))
        (loop for element across vector
              do (assert (rationalp element))
              unless (eql element 0) do
                (setf identity-p nil))
        (values vector identity-p))))

(defun free-variables (form &optional environment)
  (let (result)
    (agnostic-lizard:walk-form
     form environment
     :on-every-atom
     (lambda (form env)
       (prog1 form
         (when (and (symbolp form)
                    (not (find form (agnostic-lizard:metaenv-variable-like-entries env)
                               :key #'first)))
           (pushnew form result)))))
    result))

(defmacro τ (input-forms output-forms)
  (flet ((constraint (input-form)
           (etypecase input-form
             (integer input-form)
             (symbol nil)))
         (variable (input-form)
           (etypecase input-form
             (integer (gensym))
             (symbol input-form))))
    (let* ((input-mask
             (map 'vector #'constraint input-forms))
           (variables
             (map 'list #'variable input-forms)))
      `(make-transformation-from-function
        (lambda ,variables
          (declare (ignorable ,@variables))
          (values ,@output-forms))
        ,input-mask))))

(defun make-transformation-from-function
    (function &optional (input-mask nil input-mask-p))
  (let* ((input-rank
           (if input-mask-p
               (length input-mask)
               (petalisp.type-codes::function-arity function)))
         (input-mask
           (if (not input-mask-p)
               (make-array input-rank :initial-element nil)
               (coerce input-mask 'simple-vector))))
    (assert (= input-rank (length input-mask)) ()
      "~@<Received the input constraints ~W of length ~D ~
          for a function with ~D arguments.~:@>"
      input-mask (length input-mask) input-rank)
    (let ((args (map 'list (lambda (constraint) (or constraint 0)) input-mask))
          ;; F is applied to many slightly different arguments, so we build a
          ;; vector pointing to the individual conses of ARGS for fast random
          ;; access.
          (arg-conses (make-array input-rank)))
      (loop for arg-cons on args
            for index from 0 do
              (setf (aref arg-conses index) arg-cons))
      ;; Initially x is the zero vector (except for input constraints, which
      ;; are ignored by A), so f(x) = Ax + b = b
      (let* ((offsets (multiple-value-call #'vector (apply function args)))
             (output-rank (length offsets))
             (output-mask (make-array output-rank :initial-element nil))
             (scalings (make-array output-rank :initial-element 0)))
        ;; Set one input at a time from zero to one (ignoring those with
        ;; constraints) and check how it changes the output.
        (loop for input-constraint across input-mask
              for arg-cons across arg-conses
              for column-index from 0
              when (not input-constraint) do
                (setf (car arg-cons) 1)
                ;; Find the row of A corresponding to the mutated input.
                ;; It is the only output that differs from b.
                (let ((outputs (multiple-value-call #'vector (apply function args))))
                  (loop for output across outputs
                        for offset across offsets
                        for row-index from 0
                        when (/= output offset) do
                          (setf (aref output-mask row-index) column-index)
                          (setf (aref scalings row-index) (- output offset))
                          (return)))
                ;; Restore the argument to zero.
                (setf (car arg-cons) 0))
        ;; Finally, check whether the derived transformation behaves like
        ;; the original function and signal an error if not.
        (let ((transformation
                (make-transformation
                 :input-rank input-rank
                 :output-rank output-rank
                 :input-mask input-mask
                 :offsets offsets
                 :scalings scalings
                 :output-mask output-mask)))
          (loop for arg-cons on args
                for input-constraint across input-mask
                when (not input-constraint) do
                  (setf (car arg-cons) 2))
          (assert (equalp (transform args transformation)
                          (multiple-value-list (apply function args)))
                  ()
                  "~@<The function ~W is not affine-linear.~:@>"
                  function)
          transformation)))))

(define-compiler-macro make-transformation-from-function
    (&whole whole &environment environment
            function &optional (input-mask nil input-mask-p))
  (if (or (not (constantp input-mask environment))
          (free-variables function))
      whole
      `(load-time-value
        (locally ;; avoid infinite compiler macro recursion
            (declare (notinline make-transformation-from-function))
          (make-transformation-from-function
           ,function
           ,@(when input-mask-p `(,input-mask))))))
  whole)
