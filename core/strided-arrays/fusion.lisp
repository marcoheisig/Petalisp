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
  (loop for input in inputs do
    (unless (strided-array-p input)
      (error "~@<Expected a strided array, but received ~S~:@>" input)))
  (trivia:match inputs
    ((list)
     (error "Cannot create a fusion with zero inputs."))
    ((list input)
     (values))
    ((list* _ _ _)
     (unless (identical inputs :key #'rank)
       (error
        "~@<The shapes of the arguments to a fusion operation must ~
            have the same rank, but the supplied arguments have the ~
            ranks ~{~#[~;~S~;~S and ~S~:;~@{~S~#[~;, and ~:;, ~]~}~]~}.~:@>"
        (remove-duplicates (mapcar #'rank inputs))))
     (map-combinations
      (lambda (two-inputs)
        (destructuring-bind (input-1 input-2) two-inputs
          (let ((shape-1 (shape input-1))
                (shape-2 (shape input-2)))
            (demand (not (set-intersectionp shape-1 shape-2))
              "~@<The index shapes of the arguments to a fusion operation ~
                must be disjoint, but shape ~S and shape ~S have the ~
                common subshape ~S.~:@>"
              shape-1
              shape-2
              (set-intersection shape-1 shape-2)))))
      inputs :length 2 :copy nil))))

(defmethod make-fusion (inputs)
  (trivia:ematch inputs
    ((list input) input)
    ((list* _ _ _)
     (make-instance 'fusion
       :element-type (atomic-type
                      (upgraded-array-element-type
                       `(or ,@(mapcar #'element-type inputs))))
       :inputs inputs
       :shape (shape-union (mapcar #'shape inputs))))))
