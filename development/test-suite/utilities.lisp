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
         (dimension (dimension strided-array))
         (generator (make-integer-generator :lower-limit -20 :upper-limit 21)))
    (reshape strided-array
             (make-transformation
              :input-dimension dimension
              :output-dimension dimension
              :translation (loop repeat dimension collect (funcall generator))
              :permutation (shuffle (iota dimension))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Equality

(defgeneric approximately-equal (value-1 value-2))

(defmethod approximately-equal ((immediate-1 immediate) (immediate-2 immediate))
  (approximately-equal (storage immediate-1) (storage immediate-2)))

(defmethod approximately-equal ((array-1 array) (array-2 array))
  (and (equal (array-dimensions array-1)
              (array-dimensions array-2))
       (loop for index below (array-total-size array-1)
             always (approximately-equal
                     (row-major-aref array-1 index)
                     (row-major-aref array-2 index)))))

(defmethod approximately-equal ((object-1 t) (object-2 t))
  (eql object-1 object-2))

(defmethod approximately-equal ((a single-float) (b single-float))
  (< (abs (- a b)) (* 64 single-float-epsilon)))

(defmethod approximately-equal ((a double-float) (b double-float))
  (< (abs (- a b)) (* 64 double-float-epsilon)))
