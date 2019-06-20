;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(define-condition specialization-error (error)
  ((%stack :initform '() :accessor specialization-error-stack))
  (:report (lambda (condition stream)
             (format stream
                     "Specialization with the wrong number of arguments, ~
                      or with arguments of the wrong type.~
                      ~%~%Specialization trace:~%~{~S~%~}"
                     (specialization-error-stack condition)))))

(defun abort-specialization ()
  (error 'specialization-error))

(defmacro with-specialization-error-frame (entry &body body)
  `(handler-case (progn ,@body)
     (specialization-error (specialization-error)
       (push ,entry (specialization-error-stack specialization-error))
       (error specialization-error))))
