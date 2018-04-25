;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(uiop:define-package :petalisp/utilities/request
  (:use :closer-common-lisp :alexandria :bordeaux-threads)
  (:export #:make-request #:request? #:wait #:complete))

(in-package :petalisp/utilities/request)

(defstruct (request
            (:constructor make-request ())
            (:copier nil)
            (:predicate request?))
  "Something to wait for in a multithreaded environment."
  (status :pending :type (member :pending :completed))
  (cvar (make-condition-variable))
  (lock (make-lock)))

(defun wait (request)
  "Wait until REQUEST is completed."
  (prog1 request
    (unless (eq (request-status request) :completed)
      (with-lock-held ((request-lock request))
        (loop until (eq (request-status request) :completed)
              do (condition-wait (request-cvar request)
                                 (request-lock request)))))))

(defun complete (request)
  "Signal the completion of REQUEST to whom it may concern."
  (prog1 request
    (with-lock-held ((request-lock request))
      (setf (request-status request) :completed)
      (condition-notify (request-cvar request)))))
