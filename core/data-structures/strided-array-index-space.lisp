;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(uiop:define-package :petalisp/core/data-structures/strided-array-index-space
  (:use :closer-common-lisp :alexandria :trivia)
  (:use
   :petalisp/utilities/all
   :petalisp/core/error-handling
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/data-structure
   :petalisp/core/data-structures/index-space)
  (:export
   #:strided-array-index-space
   #:ranges))

(in-package :petalisp/core/data-structures/strided-array-index-space)

(defclass strided-array-index-space (index-space)
  ((%ranges :initarg :ranges
            :reader ranges
            :type vector)))

(defmethod make-load-form ((object strided-array-index-space) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defmethod common-broadcast-space ((space strided-array-index-space) &rest more-spaces)
  (let* ((list-of-ranges
           (list* (ranges space)
                  (mapcar #'ranges more-spaces)))
         (longest-ranges
           (loop for longest = (first list-of-ranges) then longest
                 for maxlen = (length longest) then maxlen
                 for current in (rest list-of-ranges)
                 for length = (length current)
                 when (> length maxlen) do
                   (setf longest current)
                   (setf maxlen length)
                 finally (return longest)))
         (result-ranges
           (copy-array longest-ranges)))
    (loop for ranges in list-of-ranges
          for argument from 0 do
            (loop for range across ranges
                  for broadcast-range across result-ranges
                  for dimension from 0 do
                    (cond
                      ((equalp range broadcast-range)) ; NOP
                      ((size-one-range-p range))       ; NOP
                      ((size-one-range-p broadcast-range)
                       (setf (aref result-ranges dimension) range))
                      (t
                       (demand nil
                         "~@<There is no common broadcast space for the spaces ~
                                 ~{~#[~;and ~S~;~S ~:;~S, ~]~}.~:@>"
                         (cons space more-spaces))))))
    (make-instance 'strided-array-index-space
      :ranges result-ranges)))

(defmethod index-space-difference ((space-1 strided-array-index-space)
                                   (space-2 strided-array-index-space))
  (if-let ((intersection (index-space-intersection space-1 space-2)))
    (let ((result '()))
      (loop for r1 across (ranges space-1)
            for r2 across (ranges space-2)
            for i from 0 do
              (loop for difference in (range-difference r1 r2) do
                (let ((ranges (copy-array (ranges space-1))))
                  (replace ranges (ranges intersection) :end1 i)
                  (setf (aref ranges i) difference)
                  (push (make-instance 'strided-array-index-space :ranges ranges)
                        result)))
            finally (return result)))
    (list space-1)))

(defmethod dimension ((object strided-array-index-space))
  (length (ranges object)))

(defmethod enlarge-index-space
    ((from strided-array-index-space)
     (to strided-array-index-space))
  (let ((new-ranges (copy-array (ranges to))))
    (replace new-ranges (ranges from))
    (make-instance 'strided-array-index-space
      :ranges new-ranges)))

(defmethod index-space-equality ((object-1 strided-array-index-space)
                                 (object-2 strided-array-index-space))
  (and (= (dimension object-1) (dimension object-2))
       (every #'equalp
              (ranges object-1)
              (ranges object-2))))

(defmethod index-space-intersection ((space-1 strided-array-index-space)
                                     (space-2 strided-array-index-space))
  (make-instance 'strided-array-index-space
    :ranges
    (map 'vector
         (lambda (a b)
           (or (range-intersection a b)
               (return-from index-space-intersection nil)))
         (ranges space-1)
         (ranges space-2))))

(defmethod index-space-intersection-p
    ((space-1 strided-array-index-space)
     (space-2 strided-array-index-space))
  (loop for range-1 across (ranges space-1)
        for range-2 across (ranges space-2)
        always (range-intersection? range-1 range-2)))

(defmethod generic-unary-funcall :before
    ((transformation transformation)
     (index-space strided-array-index-space))
  (when-let ((input-constraints (input-constraints transformation)))
    (loop for range across (ranges index-space)
          for constraint across input-constraints
          for index from 0 do
            (unless (not constraint)
              (demand (and (= constraint (range-start range))
                           (= constraint (range-end range)))
                "~@<The ~:R dimension of the space ~W violates ~
                    the input constraint ~W of the transformation ~W.~:@>"
                index index-space constraint transformation)))))

(defmethod generic-unary-funcall ((transformation hairy-transformation)
                                  (index-space strided-array-index-space))
  (let ((output-ranges (make-array (output-dimension transformation)))
        (input-ranges (ranges index-space)))
    (flet ((store-output-range (output-index input-index scaling offset)
             (setf (svref output-ranges output-index)
                   (if (not input-index)
                       (make-range offset 1 offset)
                       (let ((input-range (svref input-ranges input-index)))
                         (make-range
                          (+ offset (* scaling (range-start input-range)))
                          (* scaling (range-step input-range))
                          (+ offset (* scaling (range-end input-range)))))))))
      (map-transformation-outputs transformation #'store-output-range))
    (make-instance 'strided-array-index-space
      :ranges output-ranges)))

(defmethod size ((object strided-array-index-space))
  (reduce #'* (ranges object) :key #'range-size))

(defun index-space-union-range-oracle (&rest ranges)
  (declare (dynamic-extent ranges))
  ;; determine the bounding box
  (loop for range in ranges
        minimize (range-start range) into global-start
        maximize (range-end range) into global-end
        finally
           (return
             (if (= global-start global-end)
                 (first ranges)
                 ;; now determine the step size
                 (let ((step-size (- global-end global-start)))
                   (dolist (range ranges)
                     (flet ((check (n)
                              (setf step-size
                                    (min step-size
                                         (- n global-start)))))
                       (if (> (range-start range) global-start)
                           (check (range-start range))
                           (unless (size-one-range-p range)
                             (check (+ (range-start range)
                                       (range-step range)))))))
                   (make-range global-start step-size global-end))))))

(defmethod index-space-union
    ((space-1 strided-array-index-space) &rest more-spaces)
  (make-instance 'strided-array-index-space
    :ranges (apply #'map 'vector
                   #'index-space-union-range-oracle
                   (ranges space-1) (mapcar #'ranges more-spaces))))

(defmethod index-space-union :around
    ((space-1 strided-array-index-space) &rest more-spaces)
  (let ((union (call-next-method)))
    (flet ((proper-subspace-p (space)
             (subspace-p space union)))
      (assert (proper-subspace-p space-1))
      (assert (every #'proper-subspace-p more-spaces))
      union)))

(defmethod print-object ((object strided-array-index-space) stream)
  (flet ((range-list (range)
           (list (range-start range)
                 (range-step range)
                 (range-end range))))
    (print-unreadable-object (object stream :type t)
      (princ (map 'list #'range-list (ranges object)) stream))))

(defmethod canonicalize-index-space ((list list))
  (flet ((canonicalize-range-specifier (range-specifier)
           (match range-specifier
             ((list start step end)
              (make-range start step end))
             ((list start end)
              (make-range start 1 end))
             ((list start)
              (once-only (start)
                (make-range start 1 start)))
             (length
              (make-range 0 1 (1- length))))))
    (make-instance 'strided-array-index-space
      :ranges (map 'vector #'canonicalize-range-specifier list))))

(defmethod canonicalize-index-space ((array array))
  (make-instance 'strided-array-index-space
    :ranges (map 'vector (lambda (end) (make-range 0 1 (1- end)))
                 (array-dimensions array))))
