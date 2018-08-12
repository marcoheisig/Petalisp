;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-reference-backend)

;;; The purpose of the reference backend is to compute reference solutions
;;; for automated testing. It is totally acceptable that this
;;; implementation is slow or eagerly consing, as long as it is obviously
;;; correct.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric evaluate (strided-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass reference-backend (backend)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod compute-immediates ((strided-arrays list) (backend reference-backend))
  (mapcar (compose #'make-strided-array #'evaluate) strided-arrays))

;;; Memoization

(defvar *memoization-table*)

(defmethod compute-immediates :around (strided-arrays (backend reference-backend))
  (let ((*memoization-table* (make-hash-table :test #'eq)))
    (call-next-method)))

(defmethod evaluate :around ((strided-array strided-array))
  (petalisp-memoization:with-hash-table-memoization (strided-array)
      *memoization-table*
    (call-next-method)))

;;; Evaluation

(defmethod evaluate ((simple-immediate simple-immediate))
  simple-immediate)

(defmethod evaluate ((scalar-immediate scalar-immediate))
  (make-simple-immediate
   (shape scalar-immediate)
   (lambda (index)
     (assert (null index))
     (storage scalar-immediate))))

(defmethod evaluate ((array-immediate array-immediate))
  (make-simple-immediate
   (shape array-immediate)
   (lambda (index)
     (apply #'aref (storage array-immediate) index))))

(defmethod evaluate ((application application))
  (let ((inputs (mapcar #'evaluate (inputs application))))
    (make-simple-immediate
     (shape application)
     (lambda (index)
       (apply (operator application)
              (mapcar (lambda (input) (iref input index)) inputs))))))

(defmethod evaluate ((directed-fold directed-fold))
  (let ((inputs (mapcar #'evaluate (inputs right-fold))))
    ;; TODO
    ))

(defun split-range (range)
  (multiple-value-bind (start step end)
      (range-start-step-end range)
    (let ((middle (floor (+ start end) 2)))
      (values (make-range start step middle)
              (make-range end step (+ middle step))))))

(defmethod evaluate ((tree-fold tree-fold))
  (let ((inputs (mapcar #'evaluate (inputs tree-fold))))
    (make-simple-immediate
     (shape tree-fold)
     (lambda (index)
       (labels ((divide-and-conquer (range)
                  (if (unary-range-p range)
                      (values-list
                       (mapcar (lambda (input)
                                 (iref input (cons (range-start range) index)))
                               inputs))
                      (multiple-value-bind (left right)
                          (split-range range)
                        (multiple-value-call (operator tree-fold)
                          (divide-and-conquer left)
                          (divide-and-conquer right))))))
         (divide-and-conquer (first (ranges (shape (first inputs))))))))))

(defmethod evaluate ((fusion fusion))
  (let ((inputs (mapcar #'evaluate (inputs fusion))))
    (make-simple-immediate
     (shape fusion)
     (lambda (index)
       (iref (find-if (lambda (input) (set-contains (shape input) index)) inputs)
             index)))))

(defmethod evaluate ((reference reference))
  (let ((input (evaluate (input reference))))
    (make-simple-immediate
     (shape reference)
     (lambda (index)
       (iref input (transform index (transformation reference)))))))
