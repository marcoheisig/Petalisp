;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(defstruct (circular-array
            (:copier nil)
            (:predicate circular-array-p)
            (:constructor make-circular-array
                (log-size
                 &aux (segment (make-array (ash 1 log-size))))))
  (log-size nil :type (integer 0 (#.(integer-length most-positive-fixnum))))
  (segment nil :type simple-vector))

(declaim (inline circular-array-size))
(defun circular-array-size (circular-array)
  (declare (circular-array circular-array))
  (ash 1 (circular-array-log-size circular-array)))

(declaim (inline circular-array-elt))
(defun circular-array-elt (circular-array index)
  (declare (circular-array circular-array))
  (declare (fixnum index))
  (svref (circular-array-segment circular-array)
         (logand index (1- (ash 1 (circular-array-log-size circular-array))))))

(declaim (inline (setf circular-array-elt)))
(defun (setf circular-array-elt) (value circular-array index)
  (declare (circular-array circular-array))
  (declare (fixnum index))
  (setf (svref (circular-array-segment circular-array)
               (logand index (1- (ash 1 (circular-array-log-size circular-array)))))
        value))

(declaim (ftype (function (circular-array fixnum fixnum) (values circular-array &optional))
                circular-array-grow))
(defun circular-array-grow (circular-array bottom top)
  (declare (circular-array circular-array))
  (declare (fixnum bottom))
  (declare (fixnum top))
  (let ((new (make-circular-array (1+ (circular-array-log-size circular-array)))))
    ;; Copy the elements from the old circular array to the new one.
    (loop for index fixnum from top below bottom do
      (setf (circular-array-elt new index)
            (circular-array-elt circular-array index)))
    new))

