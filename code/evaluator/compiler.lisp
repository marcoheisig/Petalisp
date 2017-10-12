;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun index-symbol (n)
  (let ((index-symbol-vector
          (load-time-value
           (make-array 0 :fill-pointer 0))))
    (iterate
      (for index from (fill-pointer index-symbol-vector) to n)
      (vector-push-extend (intern (format nil "I~D" index))
                          index-symbol-vector))
    (aref index-symbol-vector n)))

(defun binding-symbol (n)
  (let ((binding-symbol-vector
          (load-time-value
           (make-array 0 :fill-pointer 0))))
    (iterate
      (for index from (fill-pointer binding-symbol-vector) to n)
      (vector-push-extend (intern (format nil "A~D" index))
                          binding-symbol-vector))
    (aref binding-symbol-vector n)))

(defmacro %for (ranges body)
  (let* ((indices (iterate (for index below (length ranges))
                           (collect (index-symbol index))))
         (result `(setf (aref target ,@indices) ,body)))
    (iterate
      (for range in-vector ranges)
      (for index from 0)
      (setf result
            `(iterate (for ,(index-symbol index)
                           from ,(range-start range)
                           by   ,(range-step range)
                           to   ,(range-end range))
                      ,result)))
    result))

(defmacro %application (operator &rest arguments)
  `(funcall ,operator ,@arguments))

(defmacro %reference (transformation binding-index)
  (let ((indices
          (iterate (for index below (input-dimension transformation))
                   (collect (index-symbol index)))))
    `(aref ,(binding-symbol binding-index)
           ,@(funcall transformation indices))))
