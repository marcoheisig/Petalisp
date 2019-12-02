;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.test-suite)

(check-package '#:petalisp.type-inference)

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
        (loop for exponent in '(1 2 3 5) do
          (push (scale-float (coerce base fp-type) exponent) floats))))
    (remove-duplicates floats)))

(defparameter *test-reals*
  (append *test-integers* *test-floats*))

(defparameter *test-complex-numbers*
  (let ((complex-numbers '()))
    (loop for float in *test-floats* do
      (push (complex float float) complex-numbers)
      (push (complex float (- float)) complex-numbers))
    (remove-duplicates complex-numbers)))

(defparameter *test-numbers*
  (append *test-integers* *test-floats* *test-complex-numbers*))

(define-test ntype-of-test
  (loop for number in *test-numbers* do
    (is (typep number (petalisp.type-inference:type-specifier
                       (petalisp.type-inference:ntype-of number))))))

(define-test ntype-test
  (let ((type-specifiers (loop for number in *test-numbers*
                               collect (type-of number)
                               collect `(eql ,number))))
    (loop for type-specifier in type-specifiers do
      (is (subtypep type-specifier
                    (petalisp.type-inference:type-specifier
                     (petalisp.type-inference:ntype type-specifier)))))))

(define-test ntype-reasoning-test
  (loop for ntype-1 across petalisp.type-inference::*ntypes* do
    (loop for ntype-2 across petalisp.type-inference::*ntypes* do
      (let ((type-specifier-1
              (petalisp.type-inference:type-specifier ntype-1))
            (type-specifier-2
              (petalisp.type-inference:type-specifier ntype-2))
            (union-type-specifier
              (petalisp.type-inference:type-specifier
               (petalisp.type-inference:ntype-union ntype-1 ntype-2))))
        (flet ((matches (type)
                 (lambda (x) (typep x type))))
          (let* ((set-1 (remove-if-not (matches type-specifier-1) *test-numbers*))
                 (set-2 (remove-if-not (matches type-specifier-2) *test-numbers*))
                 (ntype-union (remove-if-not (matches union-type-specifier) *test-numbers*))
                 (explicit-union (cl:union set-1 set-2)))
            (is (subsetp explicit-union ntype-union :test #'equal))))))))

(define-test type-inference-test
  (flet ((test (function &rest args)
           (let ((predicted
                   (mapcar #'petalisp.type-inference:type-specifier
                           (multiple-value-list
                            (petalisp.type-inference:infer-ntypes
                             function
                             (mapcar #'petalisp.type-inference:ntype-of args)
                             (lambda () (values)))))))
             (handler-case
                 (let ((values
                         (multiple-value-list
                          (locally (declare (optimize (safety 3)))
                            (apply function args)))))
                   (is (<= (length predicted) (length values)))
                   (loop for value in values
                         for type in predicted do
                           (if (typep type '(cons (eql eql) (cons number null)))
                               ;; We compare numbers with =, because
                               ;; otherwise we run into trouble with the
                               ;; sign of floating-point zeros.
                               (is (= value (second type)))
                               (is (typep value type)))))
               (arithmetic-error ())))))
    (test 'apply '+ '(7 8))
    (signals error (test #'apply))
    (test #'apply #'+ 5 '(7 8))
    (test #'fdefinition '+)
    (signals error (test #'fdefinition 25))
    (test #'fboundp '+)
    (test #'funcall #'+)
    (test #'funcall #'+ 2 3)
    (test #'function-lambda-expression #'+)
    (signals error (test #'not))
    (test #'not 1)
    (signals error (test #'not 1 2))
    (test #'eq 1 2)
    (test #'some #'integerp '(1 2 3))
    (signals error (test #'some #'integerp))
    (test #'values 1 2 3 4.0)
    (test #'values-list '(1 2 3))
    (signals error (test #'values-list 42))
    ;; N-valued functions.
    (loop for fn in '(= /= + - * /) do
      (loop for number-1 in *test-numbers* do
        (test fn number-1)
        (loop for number-2 in *test-numbers* do
          (test fn number-1 number-2)
          (test fn number-1 number-2 number-1))))
    (loop for fn in '(< > <= >= min max) do
      (loop for number-1 in *test-reals* do
        (test fn number-1)
        (loop for number-2 in *test-reals* do
          (test fn number-1 number-2)
          (test fn number-1 number-2 number-1))))
    ;; Rounding.
    (loop for fn in '(floor ceiling truncate round ffloor fceiling ftruncate fround) do
      (loop for number-1 in *test-reals* do
        (test fn number-1)
        (loop for number-2 in *test-reals* do
          (test fn number-1 number-2))))
    ;; Trigonometric functions.
    (loop for fn in '(sin cos tan asin acos atan log exp sqrt) do
      (loop for number-1 in *test-numbers* do
        (test fn number-1)))))
