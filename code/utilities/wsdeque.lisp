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

(defstruct (wsdeque
            (:copier nil)
            (:predicate wsdequep)
            (:constructor make-wsdeque ()))
  ;; The bottom index is incremented whenever an element is pushed, and
  ;; decremented whenever an element is popped.  It must only be mutated by
  ;; the thread that owns the wsdeque.
  (bottom 0 :type fixnum)
  ;; The top index is incremented whenever an item is stolen from the
  ;; wsdeque.  It must only be modified using CAS operations, and it never
  ;; decreases.
  (top 0 :type fixnum)
  ;; The circular array is a container that can hold at least one more
  ;; element than the difference between bottom and top.  If a push
  ;; operation would fill the last element of the circular array, the push
  ;; operation has to replace it with a larger one first.
  (circular-array (make-circular-array 3) :type circular-array)
  ;;; TODO: This implementation shouldn't require any locks.  And the test
  ;;; suite indicates that it works correctly even without locks.
  ;;; Unfortunately, Petalisp exhibits very bizarre race condition if we
  ;;; omit the locks.
  (lock (bt:make-lock)
   :type bt:lock
   :read-only t))

(defun wsdeque-push (wsdeque value)
  "Insert VALUE at the bottom of the WSDEQUE.  Returns that value.

This operation must only be carried out by the thread that owns the WSDEQUE."
  (declare (wsdeque wsdeque))
  (bt:with-lock-held ((wsdeque-lock wsdeque))
    (let* ((bottom (wsdeque-bottom wsdeque))
           (top (wsdeque-top wsdeque))
           (ca (wsdeque-circular-array wsdeque))
           (size (- bottom top)))
      (when (>= size (1- (circular-array-size ca)))
        (let ((new (circular-array-grow ca bottom top)))
          (setf (wsdeque-circular-array wsdeque) new)
          (setf ca new)))
      (setf (circular-array-elt ca bottom) value)
      (incf (wsdeque-bottom wsdeque))
      value)))

(defun wsdeque-pop (wsdeque)
  "Remove the VALUE at the bottom of the WSDEQUE.  Returns two values: The
object that has been removed and T, or NIL and NIL.

This operation must only be carried out by the thread that owns the WSDEQUE."
  (declare (wsdeque wsdeque))
  (bt:with-lock-held ((wsdeque-lock wsdeque))
    (let* ((bottom (wsdeque-bottom wsdeque))
           (ca (wsdeque-circular-array wsdeque))
           (bottom (setf (wsdeque-bottom wsdeque) (1- bottom)))
           (top (wsdeque-top wsdeque))
           ;; We have already decremented bottom, so this is the size
           ;; assuming the bottom element has already been removed.
           (size (- bottom top)))
      (if (< size 0)
          ;; If the wsdeque is already empty, we set BOTTOM to TOP to restore
          ;; its canonical state.
          (progn
            (setf (wsdeque-bottom wsdeque) top)
            (values nil nil))
          (let ((object (circular-array-elt ca bottom)))
            ;; If the wsdeque contains more than one element, we can simply
            ;; remove that element.
            (if (> size 0)
                (values object t)
                ;; If the wsdeque contains exactly one element, we might
                ;; compete with another thread.  A CAS operation tells us
                ;; whether we get the last element or not.  Either way,
                ;; BOTTOM is set to TOP+1.
                (if (atomics:cas (wsdeque-top wsdeque) top (1+ top))
                    (progn
                      (setf (wsdeque-bottom wsdeque) (1+ top))
                      (values object t))
                    (progn
                      (setf (wsdeque-bottom wsdeque) (1+ top))
                      (values nil nil)))))))))

(defun wsdeque-steal (wsdeque)
  "Remove the VALUE at the top of the WSDEQUE.  Returns two values: The
object that has been removed and T, or NIL and NIL.

This operation is thread safe."
  (declare (wsdeque wsdeque))
  (bt:with-lock-held ((wsdeque-lock wsdeque))
    (let* ((top (wsdeque-top wsdeque))
           (bottom (wsdeque-bottom wsdeque))
           (ca (wsdeque-circular-array wsdeque))
           (size (- bottom top)))
      (if (<= size 0)
          (values nil nil)
          (let ((object (circular-array-elt ca top)))
            (if (atomics:cas (wsdeque-top wsdeque) top (1+ top))
                (values object t)
                (wsdeque-steal wsdeque)))))))

