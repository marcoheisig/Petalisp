;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(define-condition specialization-error (error)
  ((%stack :initform '() :accessor specialization-error-stack)))

(defun abort-specialization ()
  (error 'specialization-error))

(defmethod print-object ((specialization-error specialization-error) stream)
  (format stream
          "Specialization error.  Trace:~%~{~S~%~}"
          (reverse
           (specialization-error-stack specialization-error))))

(defmacro with-specialization-error-frame (entry &body body)
  `(handler-case (progn ,@body)
     (specialization-error (specialization-error)
       (push ,entry (specialization-error-stack specialization-error))
       (signal specialization-error))))
