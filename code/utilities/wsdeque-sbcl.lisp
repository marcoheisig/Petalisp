;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

;;; This file contains an implementation of a work-stealing deque.  It is
;;; closely modeled after the paper "Dynamic Circular Work-Stealing Deque"
;;; by David Chase and Yossi Lev (https://doi.org/10.1145/1073970.1073974).
;;;
;;; The work-stealing deque allows its owner to push and pop elements at
;;; the bottom, and it allows other threads to steal elements from the top.

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
  (%bottom 0 :type fixnum)
  ;; The top index is incremented whenever an item is stolen from the
  ;; wsdeque.  It must only be modified using CAS operations, and it never
  ;; decreases.
  (%top 0 :type fixnum)
  ;; The circular array is a container that can hold at least one more
  ;; element than the difference between bottom and top.  If a push
  ;; operation would fill the last element of the circular array, the push
  ;; operation has to replace it with a larger one first.
  (%circular-array (make-circular-array 3) :type circular-array))

(defmacro define-volatile-slot-wrapper (struct-name slot-name &key (read-only nil))
  (let* ((package (symbol-package struct-name))
         (external-accessor-name
           (intern (format nil "~:@(~A-~A~)" struct-name slot-name) package))
         (internal-accessor-name
           (intern (format nil "~:@(~A-%~A~)" struct-name slot-name) package)))
    `(progn
       (declaim (inline ,external-accessor-name (setf ,external-accessor-name)))
       (defun ,external-accessor-name (,struct-name)
         (sb-thread:barrier (:read))
         (,internal-accessor-name ,struct-name))
       ,@(when (not read-only)
           `((defun (setf ,external-accessor-name) (value ,struct-name)
               (sb-thread:barrier (:write))
               (setf (,internal-accessor-name ,struct-name) value)
               (sb-thread:barrier (:memory))
               value))))))

(define-volatile-slot-wrapper wsdeque bottom)
(define-volatile-slot-wrapper wsdeque top)
(define-volatile-slot-wrapper wsdeque circular-array)

(defun wsdeque-push (wsdeque value)
  "Insert VALUE at the bottom of the WSDEQUE.  Returns that value.

This operation must only be carried out by the thread that owns the WSDEQUE."
  (declare (wsdeque wsdeque))
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
    value))

(defun wsdeque-pop (wsdeque)
  "Remove the VALUE at the bottom of the WSDEQUE.  Returns two values: The
object that has been removed and T, or NIL and NIL.

This operation must only be carried out by the thread that owns the WSDEQUE."
  (declare (wsdeque wsdeque))
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
              (if (atomics:cas (wsdeque-%top wsdeque) top (1+ top))
                  (progn
                    (setf (wsdeque-bottom wsdeque) (1+ top))
                    (values object t))
                  (progn
                    (setf (wsdeque-bottom wsdeque) (1+ top))
                    (values nil nil))))))))

(defun wsdeque-steal (wsdeque)
  "Remove the VALUE at the top of the WSDEQUE.  Returns two values: The
object that has been removed and T, or NIL and NIL.

This operation is thread safe."
  (declare (wsdeque wsdeque))
  (let* ((top (wsdeque-top wsdeque))
         (bottom (wsdeque-bottom wsdeque))
         (ca (wsdeque-circular-array wsdeque))
         (size (- bottom top)))
    (if (<= size 0)
        (values nil nil)
        (let ((object (circular-array-elt ca top)))
          (if (atomics:cas (wsdeque-%top wsdeque) top (1+ top))
              (values object t)
              (wsdeque-steal wsdeque))))))
