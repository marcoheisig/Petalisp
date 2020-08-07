;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

;;; This file contains an implementation of a work-stealing deque.  It is
;;; closely modeled after the paper "Dynamic Circular Work-Stealing Deque"
;;; by David Chase and Yossi Lev (https://doi.org/10.1145/1073970.1073974).
;;;
;;; The work-stealing deque allows its owner to push and pop elements at
;;; the bottom, and it allows other threads to steal elements from the top.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Circular Array

(defstruct (circular-array
            (:copier nil)
            (:predicate circular-array-p)
            (:constructor make-circular-array
                (log-size
                 &aux (segment (make-array (ash 1 log-size))))))
  (log-size nil :type (integer 0 (#.(integer-length most-positive-fixnum))))
  (segment nil :type simple-vector))

(declaim (inline circular-array-size))
(defun circular-array-size (circular-array)
  (declare (circular-array circular-array))
  (ash 1 (circular-array-log-size circular-array)))

(declaim (inline circular-array-elt))
(defun circular-array-elt (circular-array index)
  (declare (circular-array circular-array))
  (declare (fixnum index))
  (svref (circular-array-segment circular-array)
         (logand index (1- (ash 1 (circular-array-log-size circular-array))))))

(declaim (inline (setf circular-array-elt)))
(defun (setf circular-array-elt) (value circular-array index)
  (declare (circular-array circular-array))
  (declare (fixnum index))
  (setf (svref (circular-array-segment circular-array)
               (logand index (1- (ash 1 (circular-array-log-size circular-array)))))
        value))

(declaim (ftype (function (circular-array fixnum fixnum) (values circular-array &optional))
                circular-array-grow))
(defun circular-array-grow (circular-array bottom top)
  (declare (circular-array circular-array))
  (declare (fixnum bottom))
  (declare (fixnum top))
  (let ((new (make-circular-array (1+ (circular-array-log-size circular-array)))))
    ;; Copy the elements from the old circular array to the new one.
    (loop for index fixnum from top below bottom do
      (setf (circular-array-elt new index)
            (circular-array-elt circular-array index)))
    new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Work Stealing Deque

(defstruct (deque
            (:copier nil)
            (:predicate dequep)
            (:constructor make-deque ()))
  ;; The bottom index is incremented whenever an element is pushed, and
  ;; decremented whenever an element is popped.  It must only be mutated by
  ;; the thread that owns the deque.
  (bottom 0 :type fixnum)
  ;; The top index is incremented whenever an item is stolen from the
  ;; deque.  It must only be modified using CAS operations, and it never
  ;; decreases.
  (top 0 :type fixnum)
  ;; The circular array is a container that can hold at least one more
  ;; element than the difference between bottom and top.  If a push
  ;; operation would fill the last element of the circular array, the push
  ;; operation has to replace it with a larger one first.
  (circular-array (make-circular-array 1) :type circular-array))

(defun deque-push (deque value)
  "Insert VALUE at the bottom of the DEQUE.  Returns that value.

This operation must only be carried out by the thread that owns the DEQUE."
  (declare (deque deque))
  (let* ((bottom (deque-bottom deque))
         (top (deque-top deque))
         (ca (deque-circular-array deque))
         (size (- bottom top)))
    (when (>= size (1- (circular-array-size ca)))
      (let ((new (circular-array-grow ca bottom top)))
        (setf (deque-circular-array deque) new)
        (setf ca new)))
    (setf (circular-array-elt ca bottom) value)
    (incf (deque-bottom deque))
    value))

(defun deque-pop (deque)
  "Remove the VALUE at the bottom of the DEQUE.  Returns two values: The
object that has been removed and T, or NIL and NIL.

This operation must only be carried out by the thread that owns the DEQUE."
  (declare (deque deque))
  (let* ((ca (deque-circular-array deque))
         (bottom (decf (deque-bottom deque)))
         (top (deque-top deque))
         ;; We have already decremented bottom, so this is the size
         ;; assuming the bottom element has already been removed.
         (size (- bottom top)))
    (if (< size 0)
        ;; If the deque is already empty, we set BOTTOM to TOP to restore
        ;; its canonical state.
        (progn
          (setf (deque-bottom deque) top)
          (values nil nil))
        (let ((object (circular-array-elt ca bottom)))
          ;; If the deque contains more than one element, we can simply
          ;; remove that element.
          (if (> size 0)
              (values object t)
              ;; If the deque contains exactly one element, we might
              ;; compete with another thread.  A CAS operation tells us
              ;; whether we get the last element or not.  Either way,
              ;; BOTTOM is set to TOP+1.
              (if (atomics:cas (deque-top deque) top (1+ top))
                  (progn
                    (setf (deque-bottom deque) (1+ top))
                    (values object t))
                  (progn
                    (setf (deque-bottom deque) (1+ top))
                    (values nil nil))))))))

(defun deque-steal (deque)
  "Remove the VALUE at the top of the DEQUE.  Returns two values: The
object that has been removed and T, or NIL and NIL.

This operation is thread safe."
  (declare (deque deque))
  (let* ((top (deque-top deque))
         (bottom (deque-bottom deque))
         (ca (deque-circular-array deque))
         (size (- bottom top)))
    (if (<= size 0)
        (values nil nil)
        (let ((object (circular-array-elt ca top)))
          (if (atomics:cas (deque-top deque) top (1+ top))
              (values object t)
              (deque-steal deque))))))

