;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/data-structures/strided-array-immediate
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/petalisp
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/strided-array
   :petalisp/core/data-structures/strided-array-index-space)
  (:export
   #:strided-array-immediate))

(in-package :petalisp/core/data-structures/strided-array-immediate)

(define-class strided-array-immediate (strided-array immediate)
  ((storage :type (or array null))))

(defmethod shared-initialize :after
    ((instance strided-array-immediate) slot-names &key &allow-other-keys)
  (let ((from-storage (from-storage-transformation (index-space instance))))
    (setf (slot-value instance 'from-storage) from-storage)
    (setf (slot-value instance 'to-storage) (inverse from-storage))))

(defmethod make-immediate ((array array))
  (let ((element-type (atomic-type (array-element-type array)))
        (index-space (index-space array))
        (identity (identity-transformation (dimension array))))
    (make-instance 'strided-array-immediate
      :element-type element-type
      :index-space index-space
      :storage array
      :to-storage identity
      :from-storage identity)))

(defmethod corresponding-immediate ((strided-array strided-array))
  (make-instance 'strided-array-immediate
    :element-type (element-type strided-array)
    :index-space (index-space strided-array)))

(defun from-storage-transformation (index-space)
  "Return a non-permuting, affine transformation from a zero based array
with step size one to the given INDEX-SPACE."
  (let ((ranges (ranges index-space))
        (dimension (dimension index-space)))
    (affine-transformation
     :input-dimension dimension
     :scaling (map 'vector #'range-step ranges)
     :translation (map 'vector #'range-start ranges))))

(defmethod make-immediate! ((strided-array strided-array))
  (change-class strided-array 'strided-array-immediate))

(defmethod print-object
    ((strided-array-immediate strided-array-immediate)
     stream)
  (print-unreadable-object (strided-array-immediate stream :type t :identity t)
    (princ (storage strided-array-immediate) stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constant Folding
;;;
;;; In Petalisp, constant folding works somewhat differently, because all
;;; data is constant by definition. A greedy constant folding algorithm
;;; would directly compute the roots of the scheduled data flow
;;; graph. However, since constant folding happens in the high level
;;; program, this would completely sidestep the scheduler and associated
;;; optimization.
;;;
;;; The only case where serial and parallel execution are equally fast, is
;;; for scalar values. Our conservative choice is therefore to fold only
;;; applications to scalar values. Scalar values can be either immediates
;;; of size one, or references to such immediates. Note, however, that such
;;; references need themselves not be of size one -- they may also be
;;; broadcasting references.

(defun constant-fold-application-or-nil (function first-input all-inputs)
  (labels
      ((fail () (return-from constant-fold-application-or-nil))
       (value-or-fail (input)
         (typecase input
           (strided-array-immediate
            (row-major-aref (storage input) 0))
           (strided-array-reference
            (let ((predecessor (input input)))
              (if (and (strided-array-immediate? predecessor)
                       (= 1 (size predecessor)))
                  (row-major-aref (storage predecessor) 0)
                  (fail))))
           (t (fail)))))
    (let ((arguments (mapcar #'value-or-fail all-inputs)))
      (broadcast
       (make-immediate (apply function arguments))
       (index-space first-input)))))

(defmethod application or ((function function)
                           (first-input strided-array-immediate)
                           (all-inputs list))
  "Constant-fold operations on scalar values."
  (when (= 1 (size first-input))
    (constant-fold-application-or-nil function first-input all-inputs)))

(defmethod application or ((function function)
                           (first-input strided-array-reference)
                           (all-inputs list))
  "Constant-fold operations on references to scalar values."
  (constant-fold-application-or-nil function first-input all-inputs))

