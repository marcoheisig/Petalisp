;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp error handling

(in-package :petalisp)

(define-condition petalisp-error
    (error)
  ())

(define-condition generic-petalisp-error
    (petalisp-error)
  ((%stuff :initarg :stuff :reader stuff))
  (:report
   (lambda (condition stream)
     (format stream
             "Something went wrong, but the author has not yet documented
this error condition. The offending objects are: ~s"
             (stuff condition)))))
