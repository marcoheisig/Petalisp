;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

(defstruct (queue
            (:constructor make-queue ())
            (:copier nil)
            (:predicate queuep))
  "A FIFO queue, suitable for inter-thread communication."
  (head nil :type list)
  (tail nil :type list)
  (lock (bt:make-lock))
  (cvar (bt:make-condition-variable)))

(defun enqueue (item queue)
  "Enqueue ITEM in QUEUE and return it."
  (let ((new-cons (list item)))
    (prog1 item
      (bt:with-lock-held ((queue-lock queue))
        (cond
          ((queue-tail queue)
           (setf (cdr (queue-tail queue)) new-cons)
           (setf (queue-tail queue) (cdr (queue-tail queue))))
          (t
           (setf (queue-head queue) new-cons)
           (setf (queue-tail queue) new-cons)))
        (bt:condition-notify (queue-cvar queue))))))

(defun dequeue (queue)
  "Block until an item can be taken from QUEUE and return this item."
  (bt:with-lock-held ((queue-lock queue))
    (loop until (queue-head queue)
          do (bt:condition-wait (queue-cvar queue) (queue-lock queue)))
    (prog1 (car (queue-head queue))
      (cond
        ((eq (queue-head queue) (queue-tail queue))
         (setf (queue-head queue) nil)
         (setf (queue-tail queue) nil))
        (t
         (setf (queue-head queue) (cdr (queue-head queue))))))))
