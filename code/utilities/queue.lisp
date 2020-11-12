;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

;;; This file contains an implementation of a simple, thread-safe queue.

(defstruct (queue
            (:copier nil)
            (:predicate queuep)
            (:constructor make-queue
                (&aux (head (list '.head.)) (tail head))))
  (lock (bordeaux-threads:make-lock) :type bordeaux-threads:lock)
  (head nil :type list)
  (tail nil :type list))

(defun queue-enqueue-front (queue object)
  "Inserts OBJECT at the front of QUEUE."
  (with-accessors ((lock queue-lock)
                   (head queue-head)) queue
    (bordeaux-threads:with-lock-held (lock)
      (push object (cdr head))))
  queue)

(defun queue-enqueue-back (queue object)
  "Inserts OBJECT at the back of QUEUE."
  (with-accessors ((lock queue-lock)
                   (head queue-head)
                   (tail queue-tail)) queue
    (bordeaux-threads:with-lock-held (lock)
      (let ((new-tail (list object)))
        (setf (cdr tail) new-tail)
        (setf tail new-tail))))
  queue)

(defun queue-dequeue (queue)
  "Removes and returns the element at the front of QUEUE, or return NIL if
the queue was empty.  Return a second value that is a boolean indicating
whether an object was taken from the queue or not."
  (with-accessors ((lock queue-lock)
                   (head queue-head)
                   (tail queue-tail)) queue
    (bordeaux-threads:with-lock-held (lock)
      (if (eq head tail)
          (values nil nil)
          (values (pop (cdr head)) t)))))
