;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Primary Transformation Constructors

(defun make-identity-transformation (dimension)
  (petalisp-memoization:with-vector-memoization (dimension)
    (make-instance 'identity-transformation
      :dimension dimension)))

(defun make-transformation
    (&key input-dimension output-dimension
       (input-constraints nil input-constraints-p)
       (translation nil translation-p)
       (permutation nil permutation-p)
       (scaling nil scaling-p))
  (declare (type (or null array-length) input-dimension output-dimension))
  ;; Step 1: Uniquely determine the input-dimension and output-dimension of
  ;; the transformation or signal an error.
  (flet ((check-input-dimension (value)
           (if (not input-dimension)
               (setf input-dimension value)
               (demand (= input-dimension value)
                 "~@<Contradictory input dimensions: ~D and ~D.~:@>"
                 input-dimension value)))
         (check-output-dimension (value)
           (if (not output-dimension)
               (setf output-dimension value)
               (demand (= output-dimension value)
                 "~@<Contradictory output dimensions: ~D and ~D.~:@>"
                 output-dimension value))))
    (when input-constraints-p
      (check-input-dimension (length input-constraints)))
    (when translation-p
      (check-output-dimension (length translation)))
    (when permutation-p
      (check-output-dimension (length permutation)))
    (when scaling-p
      (check-output-dimension (length scaling))))
  (demand (or input-dimension output-dimension)
    "~@<Too few arguments to derive a transformation.~:@>")
  (cond ((not output-dimension)
         (setf output-dimension input-dimension))
        ((not input-dimension)
         (setf input-dimension output-dimension)))
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
             ;; Permutations are the most complicated sequences to
             ;; check. If an output does not reference any input, the
             ;; corresponding entry in the permutation must be nil NIL. No
             ;; index must appear more than once.
             (demand (and (every (lambda (p)
                                   (or (not p)
                                       (< -1 p input-dimension)))
                                 permutation)
                          (if (not input-constraints)
                              (loop for input-index below input-dimension
                                    always (> 2 (count input-index permutation)))
                              (loop for input-index below input-dimension
                                    for input-constraint across input-constraints
                                    always
                                    (if (not input-constraint)
                                        (> 2 (count input-index permutation))
                                        (= 0 (count input-index permutation))))))
               "~@<Invalid transformation permutation: ~W~:@>"
               permutation)
             ;; A permutation is boring if it maps each index to itself.
             (if (and (every (let ((i -1))
                               (lambda (p)
                                 (eql p (incf i))))
                             permutation)
                      (= (length permutation) input-dimension output-dimension))
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
            (= input-dimension output-dimension))
       (make-identity-transformation input-dimension))
      ;; Check whether the transformation is invertible.
      ((or (null permutation)
           (= (- output-dimension (count-if #'zerop scaling))
              (- input-dimension (count-if #'numberp input-constraints))))
       (make-instance 'hairy-invertible-transformation
         :input-dimension input-dimension
         :output-dimension output-dimension
         :input-constraints input-constraints
         :scaling scaling
         :permutation permutation
         :translation translation))
      ;; Default to a hairy, non-invertible transformation.
      (t
       (make-instance 'hairy-transformation
         :input-dimension input-dimension
         :output-dimension output-dimension
         :input-constraints input-constraints
         :scaling scaling
         :permutation permutation
         :translation translation)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conversion of Functions to Transformations

(defun make-transformation-from-function
    (function &optional (input-constraints nil input-constraints-p))
  (let* ((input-dimension
           (if input-constraints-p
               (length input-constraints)
               (function-arity function)))
         (input-constraints
           (if (not input-constraints-p)
               (make-array input-dimension :initial-element nil)
               (coerce input-constraints 'simple-vector))))
    (demand (= input-dimension (length input-constraints))
      "~@<Received the input constraints ~W of length ~D ~
          for a function with ~D arguments.~:@>"
      input-constraints (length input-constraints) input-dimension)
    (let ((args (map 'list (lambda (constraint) (or constraint 0)) input-constraints))
          ;; F is applied to many slightly different arguments, so we build a
          ;; vector pointing to the individual conses of ARGS for fast random
          ;; access.
          (arg-conses (make-array input-dimension)))
      (loop for arg-cons on args
            for index from 0 do
              (setf (aref arg-conses index) arg-cons))
      ;; Initially x is the zero vector (except for input constraints, which
      ;; are ignored by A), so f(x) = Ax + b = b
      (let* ((translation (multiple-value-call #'vector (apply function args)))
             (output-dimension (length translation))
             (permutation (make-array output-dimension
                                      :element-type '(or null array-index)
                                      :initial-element nil))
             (scaling (make-array output-dimension
                                  :element-type 'rational
                                  :initial-element 0)))
        ;; Set one input at a time from zero to one (ignoring those with
        ;; constraints) and check how it changes the output.
        (loop for input-constraint across input-constraints
              for arg-cons across arg-conses
              for column-index from 0
              when (not input-constraint) do
                (setf (car arg-cons) 1)
                ;; Find the row of A corresponding to the mutated input. It
                ;; is the only output that differs from b.
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
                 :input-dimension input-dimension
                 :output-dimension output-dimension
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
