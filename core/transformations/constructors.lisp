;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/transformations/constructors
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/error-handling
   :petalisp/core/transformations/transformation
   :petalisp/core/transformations/identity-transformation
   :petalisp/core/transformations/hairy-transformation)
  (:export
   #:make-constrained-transformation
   #:make-identity-transformation
   #:make-translating-transformation
   #:make-permuting-transformation
   #:make-scaling-transformation
   #:make-constrained-transformation
   #:make-transformation-from-function
   #:τ))

(in-package :petalisp/core/transformations/constructors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Primary Transformation Constructor

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
  ;; Step 2: Check that the content of each given vector is sane and ignore
  ;; vectors whose content is boring.
  (let* ((input-constraints
           (when input-constraints-p
             (demand (every (lambda (x)
                              (typep x '(or null integer)))
                            input-constraints)
               "~@<Invalid transformation input constraints: ~W~:@>"
               input-constraints)
             (if (every #'null input-constraints)
                 nil
                 (coerce input-constraints 'simple-vector))))
         (translation
           (when translation-p
             (demand (every #'rationalp translation)
               "~@<Invalid transformation translation: ~W~:@>"
               translation)
             (if (every #'zerop translation)
                 nil
                 (coerce translation 'simple-vector))))
         (scaling
           (when scaling-p
             (demand (every #'rationalp scaling)
               "~@<Invalid transformation scaling: ~W~:@>"
               scaling)
             (if (every (lambda (x) (= x 1)) scaling)
                 nil
                 (coerce scaling 'simple-vector))))
         (permutation
           (when permutation-p
             (demand (if (null scaling)
                         (every (lambda (p)
                                  (<= 0 p (1- input-dimension)))
                                permutation)
                         (every (lambda (s p)
                                  (or (zerop s)
                                      (<= 0 p (1- input-dimension))))
                                scaling permutation))
               "~@<Invalid transformation permutation: ~W~:@>"
               permutation)
             ;; TODO detect boring permutations
             (coerce permutation 'simple-vector))))
    ;; Step 3: Create the transformation
    (cond
      ;; Check whether we have an identity transformation
      ((and (not input-constraints)
            (not translation)
            (not scaling)
            (not permutation)
            (= input-dimension output-dimension))
       (make-identity-transformation input-dimension))
      ;; Check whether the transformation is invertible.
      ((or (not permutation)
           (flet ((ignored-input-p (input-index)
                    (not (find input-index permutation))))
             (if (not input-constraints)
                 (loop for input-index below input-dimension
                       never (ignored-input-p input-index))
                 (loop for input-index below input-dimension
                       never (and (ignored-input-p input-index)
                                  (not (svref input-constraints input-index)))))))
       (make-instance 'hairy-invertible-transformation
         :input-dimension input-dimension
         :output-dimension output-dimension
         :input-constraints input-constraints
         :scaling scaling
         :permutation permutation
         :translation translation))
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
;;; Trivial Constructors

(defun make-identity-transformation (dimension)
  (with-vector-memoization (dimension)
    (make-instance 'identity-transformation
      :dimension dimension)))

(defun make-translating-transformation (translation)
  (make-transformation :translation translation))

(defun make-permuting-transformation (permutation)
  (make-transformation :permutation permutation))

(defun make-scaling-transformation (scaling)
  (make-transformation :scaling scaling))

(defun make-constrained-transformation (input-constraints)
  (make-transformation :input-constraints input-constraints))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conversion of Functions to Transformations

(defun make-transformation-from-function
    (function &optional (input-constraints nil input-constraints-p))
  (let* ((input-dimension (function-arity function))
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
            for i from 0 do
              (setf (aref arg-conses i) arg-cons))
      ;; Initially x is the zero vector (except for input constraints, which
      ;; are ignored by A), so f(x) = Ax + b = b
      (let* ((translation (multiple-value-call #'vector (apply function args)))
             (output-dimension (length translation))
             ;; now determine the scaled permutation matrix A
             (column-indices (make-array output-dimension
                                         :element-type 'number
                                         :initial-element 0))
             (scaling (make-array output-dimension
                                  :element-type 'rational
                                  :initial-element 0)))
        ;; set one input at a time from zero to one (ignoring those with
        ;; constraints) and check how it changes the result
        (loop for input-constraint across input-constraints
              for arg-cons across arg-conses
              for column-index from 0
              when (not input-constraint) do
                (setf (car arg-cons) 1)
                ;; find the row of A corresponding to the mutated input
                (let ((results (multiple-value-call #'vector (apply function args))))
                  (loop for result across results
                        for offset across translation
                        for row-index from 0
                        when (/= result offset) do
                          (setf (aref column-indices row-index) column-index)
                          (setf (aref scaling row-index) (- result offset))))
                (setf (car arg-cons) 0))
        (make-transformation
         :input-constraints input-constraints
         :translation translation
         :scaling scaling
         :permutation column-indices)))))

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
           ,@(when input-constraints-p `(,input-constraints)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The τ Macro

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
