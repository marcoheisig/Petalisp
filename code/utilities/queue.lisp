;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

;;; This file contains an implementation of a simple, thread-safe queue.
;;; It uses the two-lock concurrent queue algorithm by Michael and Scott
;;; (https://doi.org/10.1145/248052.248106).

(defstruct (queue
            (:copier nil)
            (:predicate queuep)
            (:constructor make-queue (&aux (head (list nil)) (tail head))))
  (h-lock (bordeaux-threads:make-lock "H-Lock") :type bordeaux-threads:lock)
  (t-lock (bordeaux-threads:make-lock "T-Lock") :type bordeaux-threads:lock)
  ;; Invariants:
  ;;
  ;; - HEAD and TAIL are cons cells.
  ;;
  ;; - HEAD is a cons whose CAR is NIL and whose CDR is the list of items
  ;;   in the queue.
  ;;
  ;; - TAIL is the last cons of the list of items in the queue, or HEAD, if
  ;;   the queue is empty
  (head nil :type cons)
  (tail nil :type cons))

(defun queue-enqueue (queue object)
  "Inserts OBJECT at the back of QUEUE."
  (with-accessors ((t-lock queue-t-lock)
                   (tail queue-tail)) queue
    (let ((new-tail (list object)))
      (bordeaux-threads:with-lock-held (t-lock)
        (setf (cdr tail) new-tail)
        (setf tail new-tail))))
  queue)

(defun queue-dequeue (queue)
  "Removes and returns the element at the front of QUEUE, or return NIL if
the queue was empty.  Return a second value that is a boolean indicating
whether an object was taken from the queue or not."
  (with-accessors ((h-lock queue-h-lock)
                   (head queue-head)
                   (tail queue-tail)) queue
    (bordeaux-threads:with-lock-held (h-lock)
      (if (atom (cdr head))
          (values nil nil)
          (let ((new-head (cdr head)))
            (setf head new-head)
            (values (shiftf (car new-head) nil) t))))))
