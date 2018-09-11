;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric make-fusion (inputs)
  (:method-combination optimizing-constructor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass fusion (non-immediate)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod make-fusion :check ((inputs list))
  (unless (= 1 (length inputs))
    (map-combinations
     (lambda (two-inputs)
       (destructuring-bind (input-1 input-2) two-inputs
         (let ((shape-1 (shape input-1))
               (shape-2 (shape input-2)))
           (demand (= (dimension shape-1) (dimension shape-2))
             "~@<The index shapes of the arguments to a fusion operation ~
              must have the same dimension, but the supplied arguments are ~
              ~R- and ~R-dimensional data structures.~:@>"
             (dimension shape-1)
             (dimension shape-2))
           (demand (not (set-intersectionp shape-1 shape-2))
             "~@<The index shapes of the arguments to a fusion operation ~
                must be disjoint, but shape ~S and shape ~S have the ~
                common subshape ~S.~:@>"
             shape-1
             shape-2
             (set-intersection shape-1 shape-2)))))
     inputs
     :length 2
     :copy nil)))

(defmethod make-fusion :optimize ((inputs list))
  (when (= 1 (length inputs))
    (first inputs)))

(defmethod make-fusion ((inputs list))
  (make-instance 'fusion
    :element-type (atomic-type
                   (upgraded-array-element-type
                    `(or ,@(mapcar #'element-type inputs))))
    :inputs inputs
    :shape (shape-union (mapcar #'shape inputs))))
