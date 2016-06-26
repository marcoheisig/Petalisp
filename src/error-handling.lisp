;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp error handling

(define-condition petalisp-error
    (error)
  ())

(define-condition broadcast-error
    (petalisp-error)
  ((%ranges :initarg :ranges :reader ranges))
  (:report
   (lambda (condition stream)
     (format stream
             "Incompatible ranges:~%~s"
             (ranges condition)))))

(define-condition unable-to-replicate
    (petalisp-error)
  ((%object :initarg :object :reader object)
   (%index-space :initarg :index-space :reader index-space))
  (:report
   (lambda (condition stream)
     (format stream
             "Cannot replicate ~s to shape ~s."
             (object condition)
             (index-space condition)))))
