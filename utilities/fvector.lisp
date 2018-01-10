;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp-internals)

;;; fvector - a vector with a fill pointer

(deftype fvector () `(and (vector t) (not simple-array)))

(declaim (inline fvector fvector-emptyp fvector-push fvector-pushnew fvector-delete))

(defun fvector (&optional (size 3))
  (declare (array-length size))
  (make-array size :fill-pointer 0))

(defun fvector-emptyp (fvector)
  (declare (fvector fvector))
  (zerop (fill-pointer fvector)))

(defun fvector-push (item fvector)
  (declare (fvector fvector))
  (vector-push-extend item fvector)
  fvector)

(defun fvector-pushnew (item fvector &key (key #'identity) (test #'eql))
  (declare (fvector fvector)
           (function test key))
  (unless (find item fvector :key key :test test)
    (vector-push-extend item fvector))
  fvector)

(defun fvector-delete (item fvector &key (key #'identity) (test #'eql))
  (declare (fvector fvector)
           (function test key))
  (unless (emptyp fvector)
    (let ((reference-element (funcall key item))
          (index 0)
          (last (1- (fill-pointer fvector))))
      (declare (type (integer -1 #.(1- array-dimension-limit)) index last))
      (loop while (<= index last) do
        (if (funcall test reference-element
                     (funcall key (aref fvector index)))
            (progn
              (setf (aref fvector index)
                    (aref fvector last))
              (setf (aref fvector last) nil)
              (decf last))
            (incf index)))
      (setf (fill-pointer fvector) (1+ last))
      fvector)))

(defun fvector-test (item fvector)
  (declare (fvector fvector))
  (with-unsafe-optimizations
    (loop repeat 100 do (fvector-pushnew item fvector))))
