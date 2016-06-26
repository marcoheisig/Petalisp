;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp error handling

(define-condition petalisp-error
    (error)
  ())

(define-condition dimension-not-compatible
    (petalisp-error)
  ((%ranges :initarg :ranges :reader ranges))
  (:report
   (lambda (condition stream)
     (format stream
             "Incompatible ranges:~%~s"
             (ranges condition)))))
