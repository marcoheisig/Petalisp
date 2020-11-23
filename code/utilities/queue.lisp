;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

;;; This file contains an implementation of a simple, thread-safe queue.

(defstruct (queue
            (:copier nil)
            (:predicate queuep)
            (:constructor make-queue
                (&aux (head (list '.head.)) (tail head))))
  (lock (bordeaux-threads:make-lock "Queue Lock") :type bordeaux-threads:lock)
  ;; Invariants:
  ;;
  ;; - HEAD and TAIL are always cons cells.
  ;;
  ;; - The CAR of HEAD is irrelevant.
  ;;
  ;; - HEAD points to the first cons of the list of items in the queue.
  ;;
  ;; - TAIL points to the last cons of the list of items in the queue.
  (head nil :type cons)
  (tail nil :type cons))

(defun queue-enqueue-front (queue object)
  "Inserts OBJECT at the front of QUEUE."
  (with-accessors ((lock queue-lock)
                   (head queue-head)
                   (tail queue-tail)) queue
    (bordeaux-threads:with-lock-held (lock)
      (let ((new-cons (cons object (cdr head))))
        (setf (cdr head) new-cons)
        (when (eq head tail)
          (setf tail new-cons)))))
  queue)

(defun queue-enqueue-back (queue object)
  "Inserts OBJECT at the back of QUEUE."
  (with-accessors ((lock queue-lock)
                   (head queue-head)
                   (tail queue-tail)) queue
    (bordeaux-threads:with-lock-held (lock)
      (let ((new-cons (list object)))
        (setf (cdr tail) new-cons)
        (setf tail new-cons))))
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
          (multiple-value-prog1 (values (pop (cdr head)) t)
            (when (endp (cdr head))
              (setf tail head)))))))
