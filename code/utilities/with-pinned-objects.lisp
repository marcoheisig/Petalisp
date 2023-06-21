;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(declaim (notinline touch))
(defun touch (object)
  object)

(defmacro with-pinned-objects ((&rest objects) &body body)
  #+sbcl `(sb-sys:with-pinned-objects (,@objects) ,@body)
  #-(or sbcl)
  (let ((syms (loop repeat (length objects) collect (gensym "OBJECT"))))
    `(let ,(mapcar #'list syms objects)
       ,@body
       ,@(loop for sym in syms collect `(touch ,sym)))))

(defmacro with-pinned-objects* (objects &body body)
  "Invoke BODY in an environment where all elements of the supplied sequence of
objects are pinned to their current memory location."
  `(call-with-pinned-objects ,objects (lambda () ,@body)))

(defun call-with-pinned-objects (objects thunk)
  (declare (sequence objects) (function thunk))
  (funcall
   (reduce (lambda (thunk object)
             (lambda ()
               (with-pinned-objects (object)
                 (funcall thunk))))
           objects :initial-value thunk)))
