;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-development)

(defun ndarray (n &optional (length 10))
  "Create a LENGTH^N array of double floats."
  (generate-array
   :element-type 'double-float
   :dimensions (make-list n :initial-element length)
   :element-generator (make-double-float-generator)))

(defun reshape-randomly (array)
  (let* ((strided-array (coerce-to-strided-array array))
         (rank (rank strided-array))
         (generator (make-integer-generator :lower-limit -20 :upper-limit 21)))
    (reshape strided-array
             (make-transformation
              :input-rank rank
              :output-rank rank
              :translation (loop repeat rank collect (funcall generator))
              :permutation (shuffle (iota rank))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Equality

(defgeneric approximately-equal (a b))

(defmethod approximately-equal ((a t) (b t))
  nil)

(defmethod approximately-equal ((a immediate) (b immediate))
  (approximately-equal
   (lisp-datum-from-immediate a)
   (lisp-datum-from-immediate b)))

(defmethod approximately-equal ((array-1 array) (array-2 array))
  (and (equal (array-dimensions array-1)
              (array-dimensions array-2))
       (loop for index below (array-total-size array-1)
             always (approximately-equal
                     (row-major-aref array-1 index)
                     (row-major-aref array-2 index)))))

(defmethod approximately-equal ((a t) (b t))
  (eql a b))

(defmethod approximately-equal ((a single-float) (b single-float))
  (< (abs (- a b)) (* 64 single-float-epsilon)))

(defmethod approximately-equal ((a double-float) (b double-float))
  (< (abs (- a b)) (* 64 double-float-epsilon)))
