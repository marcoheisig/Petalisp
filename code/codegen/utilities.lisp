;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.codegen)

(defun index+ (&rest fixnums)
  (apply #'+ fixnums))

(defun index* (&rest fixnums)
  (apply #'* fixnums))

(define-compiler-macro index+ (&rest forms)
  `(the fixnum
        (+ ,@(loop for form in forms collect `(the fixnum ,form)))))

(define-compiler-macro index* (&rest forms)
  `(the fixnum
        (* ,@(loop for form in forms collect `(the fixnum ,form)))))

(defmacro without-compiler-notes (&body body)
  "Suppress all compiler notes arising during the compilation of BODY."
  `(locally
       #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       ,@body))

(defmacro with-unsafe-optimization (&body body)
  "Optimize the heck out of BODY. Use with caution!

To preserve sanity, compiler efficiency hints are disabled by default. Use
WITH-UNSAFE-OPTIMIZATIONS* to see these hints."
  `(without-compiler-notes
    (with-unsafe-optimization* ,@body)))

(defmacro with-unsafe-optimization* (&body body)
  "Optimize the heck out of BODY. Use with caution!"
  (let ((settings '((speed 3) (space 0) (debug 0) (safety 0) (compilation-speed 0))))
    `(locally (declare (optimize ,@settings))
       ,@body)))

(defmacro with-debug-optimization (&body body)
  "Execute BODY safely and with highest debug settings."
  (let ((settings '((debug 3) (safety 3))))
    `(locally (declare (optimize ,@settings))
       ,@body)))

(defun unpack-array (array environment)
  (declare (array array))
  (declare (ignore environment))
  (let ((rank (array-rank array)))
    (macrolet ((adim (axis) `(array-dimension array ,axis)))
      (case rank
        (0 (values array 0))
        (1 (values array 0 1))
        (2 (values array 0 (adim 1) 1))
        (3 (values array 0 (* (adim 1) (adim 2)) (adim 2) 1))
        (4 (values array 0 (* (adim 1) (adim 2) (adim 3)) (* (adim 2) (adim 3)) (adim 3) 1))
        (otherwise
         (apply
          #'values
          array
          0
          (let ((strides '()))
            (loop for axis from rank downto 1
                  for stride = 1 then (* stride (array-dimension array axis))
                  do (push stride strides))
            strides)))))))
