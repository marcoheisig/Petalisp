;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

;;; The work-stealing deque allows its owner to push and pop elements at
;;; the bottom, and it allows other threads to steal elements from the top.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Work Stealing Deque

(defstruct (wsdeque
            (:copier nil)
            (:predicate wsdequep)
            (:constructor make-wsdeque ()))
  ;; The lock is aquired before elements are added or removed from the
  ;; queue.
  (lock (bordeaux-threads:make-lock) :type bordeaux-threads:lock)
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
  (circular-array (make-circular-array 3) :type circular-array))

(defun wsdeque-push (wsdeque value)
  "Insert VALUE at the bottom of the WSDEQUE.  Returns that value.

This operation must only be carried out by the thread that owns the WSDEQUE."
  (declare (wsdeque wsdeque))
  (with-accessors ((lock wsdeque-lock)
                   (top wsdeque-top)
                   (bottom wsdeque-bottom)
                   (circular-array wsdeque-circular-array)) wsdeque
    (bordeaux-threads:with-lock-held (lock)
      (let ((size (- bottom top)))
        (when (>= size (1- (circular-array-size circular-array)))
          (setf circular-array (circular-array-grow circular-array bottom top)))
        (setf (circular-array-elt circular-array bottom) value)
        (incf bottom)
        value))))

(defun wsdeque-pop (wsdeque)
  "Remove the VALUE at the bottom of the WSDEQUE.  Returns two values: The
object that has been removed and T, or NIL and NIL.

This operation must only be carried out by the thread that owns the WSDEQUE."
  (declare (wsdeque wsdeque))
  (with-accessors ((lock wsdeque-lock)
                   (top wsdeque-top)
                   (bottom wsdeque-bottom)
                   (circular-array wsdeque-circular-array)) wsdeque
    (bordeaux-threads:with-lock-held (lock)
      (let ((size (- bottom top)))
        (if (zerop size)
            (values nil nil)
            (values (shiftf (circular-array-elt circular-array (decf bottom)) nil) t))))))

(defun wsdeque-steal (wsdeque)
  "Remove the VALUE at the top of the WSDEQUE.  Returns two values: The
object that has been removed and T, or NIL and NIL.

This operation is thread safe."
  (declare (wsdeque wsdeque))
  (with-accessors ((lock wsdeque-lock)
                   (top wsdeque-top)
                   (bottom wsdeque-bottom)
                   (circular-array wsdeque-circular-array)) wsdeque
    (bordeaux-threads:with-lock-held (lock)
      (let ((size (- bottom top)))
        (if (zerop size)
            (values nil nil)
            (let ((object (shiftf (circular-array-elt circular-array top) nil)))
              (incf top)
              (values object t)))))))
