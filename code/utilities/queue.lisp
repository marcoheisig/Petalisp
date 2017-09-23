;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defstruct (queue
            (:constructor make-queue ())
            (:copier nil)
            (:predicate queue?))
  "A FIFO queue, suitable for inter-thread communication."
  (head nil :type list)
  (tail nil :type list)
  (lock (make-lock))
  (cvar (make-condition-variable)))

(defun enqueue (queue item)
  (let ((new-cons (list item)))
    (with-lock-held ((queue-lock queue))
      (cond ((queue-tail queue)
             (setf (cdr (queue-tail queue)) new-cons)
             (setf (queue-tail queue) (cdr (queue-tail queue))))
            (t
             (setf (queue-head queue) new-cons)
             (setf (queue-tail queue) new-cons)
             (condition-notify (queue-cvar queue))))
      item)))

(defun dequeue (queue)
  (with-lock-held ((queue-lock queue))
    (loop :until (queue-head queue)
          :do (condition-wait (queue-cvar queue) (queue-lock queue)))
    (prog1 (car (queue-head queue))
      (cond ((eq (queue-head queue) (queue-tail queue))
             (setf (queue-head queue) nil)
             (setf (queue-tail queue) nil))
            (t
             (setf (queue-head queue) (cdr (queue-head queue))))))))
