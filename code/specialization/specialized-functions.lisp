;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(defmacro defop (name function-name argument-types result-types)
  (let ((syms (loop for argument-type in argument-types
                    for index from 0
                    collect (intern (format nil "ARG-~D" index) #.*package*))))
    `(defun ,name ,syms
       (declare ,@(loop for sym in syms
                        for type in argument-types
                        collect `(type ,type ,sym)))
       (the (values ,@result-types)
            (,function-name ,@syms)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Floating-Point Operations

(defop f32.+ + (f32 f32) (f32))
(defop f64.+ + (f64 f64) (f64))
(defop c64.+ + (c64 c64) (c64))
(defop c128.+ + (c128 c128) (c128))

(defop f32.- - (f32 f32) (f32))
(defop f64.- - (f64 f64) (f64))
(defop c64.- - (c64 c64) (c64))
(defop c128.- - (c128 c128) (c128))

(defop f32.* * (f32 f32) (f32))
(defop f64.* * (f64 f64) (f64))
(defop c64.* * (c64 c64) (c64))
(defop c128.* * (c128 c128) (c128))

(defop f32./ / (f32 f32) (f32))
(defop f64./ / (f64 f64) (f64))
(defop c64./ / (c64 c64) (c64))
(defop c128./ / (c128 c128) (c128))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unsigned Integer Operations

(defmacro uN.X (n x args)
  (let ((name (intern (format nil "U~D.~A" n x) #.*package*))
        (type (intern (format nil "U~D" n) #.*package*)))
    `(defun ,name ,args
       (declare (type ,type ,@args))
       (mod (,x ,@args) (expt 2 ,n)))))

(uN.X 1 + (arg-1 arg-2))
(uN.X 2 + (arg-1 arg-2))
(uN.X 4 + (arg-1 arg-2))
(uN.X 8 + (arg-1 arg-2))
(uN.X 16 + (arg-1 arg-2))
(uN.X 32 + (arg-1 arg-2))
(uN.X 64 + (arg-1 arg-2))

(uN.X 1 * (arg-1 arg-2))
(uN.X 2 * (arg-1 arg-2))
(uN.X 4 * (arg-1 arg-2))
(uN.X 8 * (arg-1 arg-2))
(uN.X 16 * (arg-1 arg-2))
(uN.X 32 * (arg-1 arg-2))
(uN.X 64 * (arg-1 arg-2))

(uN.X 1 - (arg-1 arg-2))
(uN.X 2 - (arg-1 arg-2))
(uN.X 4 - (arg-1 arg-2))
(uN.X 8 - (arg-1 arg-2))
(uN.X 16 - (arg-1 arg-2))
(uN.X 32 - (arg-1 arg-2))
(uN.X 64 - (arg-1 arg-2))
