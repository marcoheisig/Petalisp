;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

;;; This file contains an implementation of a thread-safe set of weak
;;; references.  Items can be added explicitly, but never removed.  Items
;;; are discarded automatically once they become unreachable by any other
;;; means than the weak reference.

(defstruct (weak-set
            (:constructor make-weak-set
                (&key (test #'eql)
                 &aux (test (coerce test 'function))))
            (:predicate weak-set-p)
            (:copier nil))
  (test nil :type function :read-only t)
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
                   (test weak-set-test))
      weak-set
    (loop for old = pointers
          for new = (adjoin (trivial-garbage:make-weak-pointer item) old
                            :key #'trivial-garbage:weak-pointer-value
                            :test test)
          until (or (eq old new)
                    (atomics:cas pointers old new)))))
