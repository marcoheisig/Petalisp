;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp-internals)

(defgeneric report-condition (condition stream))

;;; It is crucial to differentiate between two kinds of errors -- those
;;; triggered by improper usage of Petalisp and those occurring even though
;;; Petalisp has been used properly. The former are expected to be far more
;;; frequent and part of the everyday user experience, while the latter
;;; should ideally never arise. Needless to say, errors triggered by the
;;; user should emit a detailed and helpful report, while the latter do not
;;; even deserve their own condition type.

(define-condition petalisp-user-error (error)
  ()
  (:report report-condition))

(define-condition application-to-data-structures-of-different-shape
    (petalisp-user-error)
  ((%data-structures :initarg :data-structures :reader data-structures)))

(define-condition reduction-of-data-structure-with-dimension-zero
    (petalisp-user-error)
  ((%data-structure :initarg :data-structure :reader data-structure)))

(define-condition fusion-of-elements-of-different-dimension
    (petalisp-user-error)
  ((%elements :initarg :elements :reader elements)))

(define-condition reference-to-non-subspace
    (petalisp-user-error)
  ((%data-structure :initarg :data-structure :reader data-structure)
   (%index-space :initarg :index-space :reader index-space)))

(define-condition reference-with-transformation-of-invalid-dimension
    (petalisp-user-error)
  ((%data-structure :initarg :data-structure :reader data-structure)
   (%transformation :initarg :transformation :reader transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition Reporters

(defmethod report-condition
    ((condition petalisp-user-error) stream)
  (format stream "~A" (class-name
                       (class-of condition))))

(defmethod report-condition
    ((condition application-to-arrays-of-different-shape) stream)
  (format stream
          "All arguments of an application must have the same shape,~@
           but the following arguments were given:~@
           ~%~{~S~%~}"
          (data-structures condition)))
