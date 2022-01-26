;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(defpackage #:petalisp.karmarkar-karp
  (:use #:common-lisp #:petalisp.utilities))

(in-package #:petalisp.karmarkar-karp)

;; A function that returns the weight of an object being processed.
(defvar *weight*)

;; The number of partitions to be created.
(defvar *k*)

(defstruct (subset
            (:constructor make-subset))
  (elements '() :type list)
  (weight 0 :type real))

(defun combine-subsets (subset1 subset2)
  (make-subset
   :elements (append (subset-elements subset1)
                     (subset-elements subset2))
   :weight (+ (subset-weight subset1)
              (subset-weight subset2))))

(defstruct (tuple
            (:constructor make-tuple))
  (subsets #() :type simple-vector)
  (weight 0 :type number))

(defun tuple-subset (tuple index)
  (svref (tuple-subsets tuple) index))

(defun tuple-from-object (object)
  (let ((subsets (make-array *k*))
        (weight (funcall *weight* object)))
    (setf (aref subsets 0)
          (make-subset
           :elements (list object)
           :weight weight))
    (loop for index from 1 below *k* do
      (setf (aref subsets index)
            (make-subset :elements '() :weight 0)))
    (make-tuple
     :subsets subsets
     :weight weight)))

(defun tuple> (tuple1 tuple2)
  (> (tuple-weight tuple1)
     (tuple-weight tuple2)))

(defun combine-tuples (tuple1 tuple2)
  (let ((subsets (make-array *k*)))
    (loop for index below *k* do
      (setf (svref subsets index)
            (combine-subsets
             (tuple-subset tuple1 index)
             (tuple-subset tuple2 (- *k* index 1)))))
    (setf subsets (sort subsets #'> :key #'subset-weight))
    (make-tuple
     :subsets subsets
     :weight (- (subset-weight (svref subsets 0))
                (subset-weight (svref subsets (1- *k*)))))))

(defun karmarkar-karp (S k &key (weight #'identity))
  "Partition the set of objects S in k subsets such that the sums of the
weights of the objects in each subset are nearly equal.

Returns a vector of length k whose elements are lists that partition S.  As
a second value, returns the difference between the sum of the weights of
the smalles partition and that of the largest partition."
  (when (null S)
    (return-from karmarkar-karp (make-list k :initial-element '())))
  (let ((*weight* weight)
        (*k* k)
        (queue (queues:make-queue :priority-queue :compare #'tuple>)))
    (dolist (object S)
      (queues:qpush queue (tuple-from-object object)))
    (loop until (= (queues:qsize queue) 1) do
      (queues:qpush queue (combine-tuples (queues:qpop queue) (queues:qpop queue))))
    (let ((result (queues:qpop queue)))
      (values
       (map 'vector #'subset-elements (tuple-subsets result))
       (tuple-weight result)))))
