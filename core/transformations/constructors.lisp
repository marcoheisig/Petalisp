;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Primary Transformation Constructors

(defun identity-transformation (rank)
  (petalisp-memoization:with-vector-memoization (rank)
    (make-instance 'identity-transformation
      :rank rank)))

(defun make-transformation
    (&key input-rank output-rank
       (input-constraints nil input-constraints-p)
       (translation nil translation-p)
       (permutation nil permutation-p)
       (scaling nil scaling-p))
  (declare (type (or null array-length) input-rank output-rank))
  ;; Step 1: Uniquely determine the input-rank and output-rank of
  ;; the transformation or signal an error.
  (flet ((check-input-rank (value)
           (if (not input-rank)
               (setf input-rank value)
               (demand (= input-rank value)
                 "~@<Contradictory input ranks: ~D and ~D.~:@>"
                 input-rank value)))
         (check-output-rank (value)
           (if (not output-rank)
               (setf output-rank value)
               (demand (= output-rank value)
                 "~@<Contradictory output ranks: ~D and ~D.~:@>"
                 output-rank value))))
    (when input-constraints-p
      (check-input-rank (length input-constraints)))
    (when translation-p
      (check-output-rank (length translation)))
    (when permutation-p
      (check-output-rank (length permutation)))
    (when scaling-p
      (check-output-rank (length scaling))))
  (demand (or input-rank output-rank)
    "~@<Too few arguments to derive a transformation.~:@>")
  (cond ((not output-rank)
         (setf output-rank input-rank))
        ((not input-rank)
         (setf input-rank output-rank)))
  ;; Step 2: Check that the content of each given sequence is sane and
  ;; ignore vectors whose content is boring.
  (let* ((input-constraints
           (when input-constraints-p
             (demand (every (lambda (x)
                              (typep x '(or null integer)))
                            input-constraints)
               "~@<Invalid transformation input constraints: ~W~:@>"
               input-constraints)
             ;; An input constraint sequence is boring if it consists of
             ;; NIL only.
             (if (every #'null input-constraints)
                 nil
                 (coerce input-constraints 'simple-vector))))
         (translation
           (when translation-p
             (demand (every #'rationalp translation)
               "~@<Invalid transformation translation: ~W~:@>"
               translation)
             ;; A translation is boring if its entries are all zero.
             (if (every #'zerop translation)
                 nil
                 (coerce translation 'simple-vector))))
         (permutation
           (when permutation-p
             ;; Permutations are the most complicated sequences to check.
             ;; If an output does not reference any input, the
             ;; corresponding entry in the permutation must be nil NIL.  No
             ;; index must appear more than once.

             ;; TODO check permutations...

             ;; A permutation is boring if it maps each index to itself.
             (if (and (every (let ((i -1))
                               (lambda (p)
                                 (eql p (incf i))))
                             permutation)
                      (= (length permutation) input-rank output-rank))
                 nil
                 (coerce permutation 'simple-vector))))
         (scaling
           (when scaling-p
             (demand (every #'rationalp scaling)
               "~@<Invalid transformation scaling: ~W~:@>"
               scaling)
             (unless (not permutation)
               (demand (every (lambda (p s)
                                (or p (zerop s)))
                              permutation
                              scaling)
                       "~@<The scaling ~W has nonzero entries in places ~
                           where the permutation ~W is NIL.~:@>"
                       scaling permutation))
             ;; A scaling is boring if it is always one
             (if (every (lambda (x) (= x 1)) scaling)
                 nil
                 (coerce scaling 'simple-vector)))))
    ;; Step 3: Create the transformation
    (cond
      ;; Check whether we have an identity transformation.
      ((and (not input-constraints)
            (not translation)
            (not scaling)
            (not permutation)
            (= input-rank output-rank))
       (identity-transformation input-rank))
      ;; Check whether the transformation is invertible.
      ((or (null permutation)
           (= (- output-rank (count-if #'zerop scaling))
              (- input-rank (count-if #'numberp input-constraints))))
       (make-instance 'hairy-invertible-transformation
         :input-rank input-rank
         :output-rank output-rank
         :input-constraints input-constraints
         :scaling scaling
         :permutation permutation
         :translation translation))
      ;; Default to a hairy, non-invertible transformation.
      (t
       (make-instance 'hairy-transformation
         :input-rank input-rank
         :output-rank output-rank
         :input-constraints input-constraints
         :scaling scaling
         :permutation permutation
         :translation translation)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conversion of Functions to Transformations

(defun make-transformation-from-function
    (function &optional (input-constraints nil input-constraints-p))
  (let* ((input-rank
           (if input-constraints-p
               (length input-constraints)
               (function-arity function)))
         (input-constraints
           (if (not input-constraints-p)
               (make-array input-rank :initial-element nil)
               (coerce input-constraints 'simple-vector))))
    (demand (= input-rank (length input-constraints))
      "~@<Received the input constraints ~W of length ~D ~
          for a function with ~D arguments.~:@>"
      input-constraints (length input-constraints) input-rank)
    (let ((args (map 'list (lambda (constraint) (or constraint 0)) input-constraints))
          ;; F is applied to many slightly different arguments, so we build a
          ;; vector pointing to the individual conses of ARGS for fast random
          ;; access.
          (arg-conses (make-array input-rank)))
      (loop for arg-cons on args
            for index from 0 do
              (setf (aref arg-conses index) arg-cons))
      ;; Initially x is the zero vector (except for input constraints, which
      ;; are ignored by A), so f(x) = Ax + b = b
      (let* ((translation (multiple-value-call #'vector (apply function args)))
             (output-rank (length translation))
             (permutation (make-array output-rank
                                      :element-type '(or null array-index)
                                      :initial-element nil))
             (scaling (make-array output-rank
                                  :element-type 'rational
                                  :initial-element 0)))
        ;; Set one input at a time from zero to one (ignoring those with
        ;; constraints) and check how it changes the output.
        (loop for input-constraint across input-constraints
              for arg-cons across arg-conses
              for column-index from 0
              when (not input-constraint) do
                (setf (car arg-cons) 1)
                ;; Find the row of A corresponding to the mutated input.
                ;; It is the only output that differs from b.
                (let ((outputs (multiple-value-call #'vector (apply function args))))
                  (loop for output across outputs
                        for offset across translation
                        for row-index from 0
                        when (/= output offset) do
                          (setf (aref permutation row-index) column-index)
                          (setf (aref scaling row-index) (- output offset))
                          (return)))
                ;; Restore the argument to zero.
                (setf (car arg-cons) 0))
        ;; Finally, check whether the derived transformation behaves like
        ;; the original function and signal an error if not.
        (let ((transformation
                (make-transformation
                 :input-rank input-rank
                 :output-rank output-rank
                 :input-constraints input-constraints
                 :translation translation
                 :scaling scaling
                 :permutation permutation)))
          (loop for arg-cons on args
                for input-constraint across input-constraints
                when (not input-constraint) do
                  (setf (car arg-cons) 2))
          (demand (equalp (transform args transformation)
                          (multiple-value-list (apply function args)))
            "~@<The function ~W is not affine-linear.~:@>"
            function)
          transformation)))))

(define-compiler-macro make-transformation-from-function
    (&whole whole &environment environment
            function &optional (input-constraints nil input-constraints-p))
  (if (or (not (constantp input-constraints environment))
          (free-variables function))
      whole
      `(load-time-value
        (locally ;; avoid infinite compiler macro recursion
            (declare (notinline make-transformation-from-function))
          (make-transformation-from-function
           ,function
           ,@(when input-constraints-p `(,input-constraints))))))
  whole)

(defmethod ensure-transformation ((function function))
  (make-transformation-from-function function))

(defmacro τ (input-forms output-forms)
  (flet ((constraint (input-form)
           (etypecase input-form
             (integer input-form)
             (symbol nil)))
         (variable (input-form)
           (etypecase input-form
             (integer (gensym))
             (symbol input-form))))
    (let* ((input-constraints
             (map 'vector #'constraint input-forms))
           (variables
             (map 'list #'variable input-forms)))
      `(make-transformation-from-function
        (lambda ,variables
          (declare (ignorable ,@variables))
          (values ,@output-forms))
        ,input-constraints))))

