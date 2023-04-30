;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(defpackage #:petalisp.cqueue
  (:use #:common-lisp)
  (:export
   #:cqueue
   #:cqueuep
   #:make-cqueue
   #:cqueue-enqueue
   #:cqueue-dequeue))

(in-package #:petalisp.cqueue)

;;; This file contains an implementation of a simple, thread-safe queue.
;;; It uses the two-lock concurrent queue algorithm by Michael and Scott
;;; (https://doi.org/10.1145/248052.248106).

(defstruct (cqueue
            (:copier nil)
            (:predicate cqueuep)
            (:constructor make-cqueue (&aux (head (list nil)) (tail head))))
  (h-lock (bordeaux-threads:make-lock "H-Lock") :type bordeaux-threads:lock)
  (t-lock (bordeaux-threads:make-lock "T-Lock") :type bordeaux-threads:lock)
  ;; Invariants:
  ;;
  ;; - HEAD and TAIL are cons cells.
  ;;
  ;; - HEAD is a cons whose CAR is NIL and whose CDR is the list of items
  ;;   in the cqueue.
  ;;
  ;; - TAIL is the last cons of the list of items in the cqueue, or HEAD, if
  ;;   the cqueue is empty
  (head nil :type cons)
  (tail nil :type cons))

(defun cqueue-enqueue (cqueue object)
  "Inserts OBJECT at the back of CQUEUE."
  (with-accessors ((t-lock cqueue-t-lock)
                   (tail cqueue-tail)) cqueue
    (let ((new-tail (list object)))
      (bordeaux-threads:with-lock-held (t-lock)
        (setf (cdr tail) new-tail)
        (setf tail new-tail))))
  cqueue)

(defun cqueue-dequeue (cqueue)
  "Removes and returns the element at the front of CQUEUE, or return NIL if the
cqueue was empty.  Return a second value that is a boolean indicating whether
an object was taken from the cqueue or not."
  (with-accessors ((h-lock cqueue-h-lock)
                   (head cqueue-head)
                   (tail cqueue-tail)) cqueue
    (bordeaux-threads:with-lock-held (h-lock)
      (if (atom (cdr head))
          (values nil nil)
          (let ((new-head (cdr head)))
            (setf head new-head)
            (values (shiftf (car new-head) nil) t))))))
