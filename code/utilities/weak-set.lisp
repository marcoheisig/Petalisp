;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

;;; This file contains an implementation of a thread-safe set of weak
;;; references.  Items can be added explicitly, but never removed.  Items
;;; are discarded automatically once they become unreachable by any other
;;; means than the weak reference.

(defstruct (weak-set
            (:constructor make-weak-set
                (&key (test #'eql) (max-size nil) (min-size 1)
                 &aux (test (coerce test 'function))))
            (:predicate weak-set-p)
            (:copier nil))
  (test nil :type function :read-only t)
  ;; If MAX-SIZE is non-null, it denotes the length of the weak set at
  ;; which the set starts to discard all but its MIN-SIZE youngest items.
  ;; This way, the size of the weak set can be bounded at the expense of
  ;; the set not being precise anymore.
  (max-size nil :type (or null unsigned-byte) :read-only t)
  (min-size nil :type (integer 1 *) :read-only t)
  (pointers '()))

(defun cleanup-weak-set (weak-set)
  (declare (weak-set weak-set))
  (with-accessors ((pointers weak-set-pointers)) weak-set
    (loop for old = pointers
          for new = (remove-if-not #'trivial-garbage:weak-pointer-value old)
          until (or (eq new old)
                    (atomics:cas pointers old new)))))

(defun map-weak-set (function weak-set)
  (declare (function function))
  (declare (weak-set weak-set))
  (loop with cleanup = nil
        for pointer in (weak-set-pointers weak-set)
        for value = (trivial-garbage:weak-pointer-value pointer)
        if (null value)
          do (setf cleanup t)
        else
          do (funcall function value)
        finally
           (when cleanup
             (cleanup-weak-set weak-set))))

(defun weak-set-size (weak-set)
  (declare (weak-set weak-set))
  (let ((counter 0))
    (flet ((incf-counter (x)
             (declare (ignore x))
             (incf counter)))
      (declare (dynamic-extent #'incf-counter))
      (map-weak-set #'incf-counter weak-set))
    counter))

(defun weak-set-add (weak-set item)
  (declare (weak-set weak-set))
  (assert (not (null item)))
  (with-accessors ((pointers weak-set-pointers)
                   (min-size weak-set-min-size)
                   (max-size weak-set-max-size)
                   (test weak-set-test))
      weak-set
    (loop for old = pointers
          for cleanup = nil
          for size fixnum = 0
          for new = (progn
                      (loop for weak-pointer in old
                            for object = (trivial-garbage:weak-pointer-value weak-pointer) do
                              (if (null object)
                                  (setf cleanup t)
                                  (if (funcall test object item)
                                      (return-from weak-set-add item)
                                      (incf size))))
                      (if (or (not max-size)
                              (< size max-size))
                          (cons (trivial-garbage:make-weak-pointer item)
                                old)
                          (cons (trivial-garbage:make-weak-pointer item)
                                (subseq old 0 (1- min-size)))))
          until (atomics:cas pointers old new))))
