;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

;;; This file contains an implementation of a simple FIFO queue.

(defstruct (queue
            (:copier nil)
            (:predicate queuep)
            (:constructor make-queue
                (&aux (head (list '.head.)) (tail head))))
  (head nil :type list)
  (tail nil :type list))

(defun queue-enqueue-front (queue object)
  "Insert OBJECT at the front of QUEUE."
  (push object (cdr (queue-head queue)))
  queue)

(defun queue-enqueue-back (queue object)
  "Insert OBJECT at the back of QUEUE."
  (symbol-macrolet ((head (queue-head queue))
                    (tail (queue-tail queue)))
    (let ((new-tail (list object)))
      (setf (cdr tail) new-tail)
      (setf tail new-tail)))
  queue)

(defun queue-dequeue (queue)
  "Remove and return the element at the front of QUEUE, or return NIL if
the queue was empty.  Return a second value that is a boolean indicating
whether an object was taken from the queue or not."
  (if (queue-empty-p queue)
      (values nil nil)
      (values (pop (cdr (queue-head queue))) t)))

(defun queue-empty-p (queue)
  "Return T if the queue is empty, and NIL otherwise."
  (eq (queue-head queue)
      (queue-tail queue)))
