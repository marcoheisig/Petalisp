;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

(defparameter *test-integers*
  (let ((integers '()))
    (loop for integer in (list -1338 -1337 -19 -3 0 3 19 1337 1338) do
      (push integer integers))
    (loop for bits = 1 then (* bits 2) until (>= bits 64) do
      (push (1+ (+ (expt 2 bits))) integers)
      (push (1- (+ (expt 2 bits))) integers)
      (push (+ (expt 2 bits)) integers)
      (push (- (expt 2 bits)) integers)
      (push (1- (- (expt 2 bits))) integers)
      (push (1+ (- (expt 2 bits))) integers))
    (remove-duplicates integers)))

(defparameter *test-floats*
  (let ((floats '()))
    (push most-positive-long-float floats)
    (push most-positive-double-float floats)
    (push most-positive-single-float floats)
    (push most-positive-short-float floats)
    (push most-negative-short-float floats)
    (push most-negative-single-float floats)
    (push most-negative-double-float floats)
    (push most-negative-long-float floats)
    (loop for base in (list -0.7L0 -0.1L0 -0.0L0 +0.0L0 +0.1L0 +0.7L0) do
      (loop for fp-type in '(short-float single-float double-float long-float) do
        (loop for exponent in (list 1 2 3 5) do
          (push (scale-float (coerce base fp-type) exponent) floats))))
    (remove-duplicates floats)))

(defparameter *test-complex-numbers*
  (let ((complex-numbers '()))
    (loop for float in *test-floats* do
      (push (complex float float) complex-numbers)
      (push (complex float (- float)) complex-numbers))
    (remove-duplicates complex-numbers)))

(defparameter *test-numbers*
  (append *test-integers* *test-floats* *test-complex-numbers*))

(test type-code-of-test
  (loop for number in *test-numbers* do
    (is (typep number (petalisp.type-codes:type-specifier-from-type-code
                       (petalisp.type-codes:type-code-of number))))))

(test type-code-from-type-specifier-test
  (let ((type-specifiers (loop for number in *test-numbers*
                               collect (type-of number)
                               collect `(eql ,number))))
    (loop for type-specifier in type-specifiers do
      (is (subtypep type-specifier
                    (petalisp.type-codes:type-specifier-from-type-code
                     (petalisp.type-codes:type-code-from-type-specifier type-specifier)))))))

(test type-code-reasoning-test
  (loop for type-code-1 below petalisp.type-codes:type-code-limit do
    (loop for type-code-2 below petalisp.type-codes:type-code-limit do
      (let ((type-specifier-1
              (petalisp.type-codes:type-specifier-from-type-code type-code-1))
            (type-specifier-2
              (petalisp.type-codes:type-specifier-from-type-code type-code-2))
            (union-type-specifier
              (petalisp.type-codes:type-specifier-from-type-code
               (petalisp.type-codes:type-code-union type-code-1 type-code-2))))
        (flet ((matches (type)
                 (lambda (x) (typep x type))))
          (let* ((set-1 (remove-if-not (matches type-specifier-1) *test-numbers*))
                 (set-2 (remove-if-not (matches type-specifier-2) *test-numbers*))
                 (type-code-union (remove-if-not (matches union-type-specifier) *test-numbers*))
                 (explicit-union (cl:union set-1 set-2)))
            (is (subsetp explicit-union type-code-union :test #'equal))))))))

(test type-inference-test
  (flet ((test (function &rest args)
           (let ((predicted
                   (mapcar #'petalisp.type-codes:type-specifier-from-type-code
                           (multiple-value-list
                            (apply #'petalisp.type-codes:values-type-codes
                                   function
                                   (mapcar #'petalisp.type-codes:type-code-of args))))))
             (if (some #'null predicted)
                 (signals error (locally (declare (optimize (safety 3)))
                                   (apply function args)))
                 (loop for value in (multiple-value-list (apply function args))
                       for type in predicted do
                         (is (typep value type)))))))
    (test #'apply #'apply)
    (test #'apply #'+ 5 '(7 8))
    (test #'fdefinition '+)
    (test #'fdefinition 25)
    (test #'fboundp '+)
    (test #'fboundp 42)
    (test #'funcall #'+)
    (test #'funcall #'+ 2 3)
    (test #'function-lambda-expression #'+)
    (test #'not)
    (test #'not 1)
    (test #'not 1 2)
    (test #'eq 1 2)
    (test #'some #'integerp '(1 2 3))
    (test #'some #'integerp)
    (test #'values 1 2 3 4.0)
    (test #'values-list '(1 2 3))
    (test #'values-list 42)))
