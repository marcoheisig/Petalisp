;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(declaim (notinline touch))
(defun touch (object) object)

(defmacro with-pinned-objects ((&rest objects) &body body)
  #+sbcl `(sb-sys:with-pinned-objects ,objects ,@body)
  #-(or sbcl)
  (let ((thunk (gensym "THUNK"))
        (symbols (loop repeat (length objects) collect (gensym))))
    `(flet ((,thunk () ,@body))
       (let ,(mapcar #'list symbols objects)
         (declare (dynamic-extent ,@symbols))
         (multiple-value-prog1 (,thunk)
           ,@(loop for symbol in symbols
                   collect `(touch ,symbol)))))))
