;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.blueprint-compiler)

;;; We need this macro because the blueprint compiler can only handle forms
;;; that are flat and would thus destroy (SETF (AREF ...) ...) forms by
;;; hoisting the AREF subform.
(defmacro store (value array row-major-index)
  `(setf (row-major-aref ,array ,row-major-index)
         ,value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Working with addresses and loops

(defun fixnum-+ (&rest fixnums)
  (apply #'+ fixnums))

(defun fixnum-* (&rest fixnums)
  (apply #'* fixnums))

(define-compiler-macro fixnum-+ (&rest forms)
  `(the fixnum
        (+ ,@(loop for form in forms collect `(the fixnum ,form)))))

(define-compiler-macro fixnum-* (&rest forms)
  `(the fixnum
        (* ,@(loop for form in forms collect `(the fixnum ,form)))))

(defun i+ (&rest expressions)
  (trivia:match (remove 0 expressions)
    ((list) 0)
    ((list expression) expression)
    (expressions `(fixnum-+ ,@expressions))))

(defun i* (&rest expressions)
  (trivia:match (remove 1 expressions)
    ((list) 1)
    ((list expression) expression)
    ((trivia:guard expressions (member 0 expressions)) 0)
    (expressions `(fixnum-* ,@expressions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Optimization

(defmacro without-compiler-notes (&body body)
  "Suppress all compiler notes arising during the compilation of BODY."
  `(locally
       #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       ,@body))

(defmacro with-unsafe-optimizations* (&body body)
  "Optimize the heck out of BODY. Use with caution!"
  (let ((settings '((speed 3) (space 0) (debug 0) (safety 0) (compilation-speed 0))))
    `(locally (declare (optimize ,@settings))
       ,@body)))

(defmacro with-unsafe-optimizations (&body body)
  "Optimize the heck out of BODY. Use with caution!

To preserve sanity, compiler efficiency hints are disabled by default. Use
WITH-UNSAFE-OPTIMIZATIONS* to see these hints."
  `(without-compiler-notes
    (with-unsafe-optimizations* ,@body)))

(defmacro dx-let (bindings &body body)
  "Like LET, but declare every variable to have dynamic extent."
  `(let ,bindings
     (declare (dynamic-extent
               ,@(mapcar (trivia:lambda-ematch
                           ((list var _) var)
                           (var var))
                         bindings)))
     ,@body))

(defmacro dx-let* (bindings &body body)
  "Like LET*, but declare every variable to have dynamic extent."
  `(let* ,bindings
     (declare (dynamic-extent
               ,@(mapcar (trivia:lambda-ematch
                           ((list var _) var)
                           ((list var) var)
                           (var var))
                         bindings)))
     ,@body))

(defmacro dx-flet (functions &body body)
  "Like FLET, but declare every function to have dynamic extent."
  `(flet ,functions
     (declare (dynamic-extent
               ,@(mapcar (trivia:lambda-ematch
                           ((list* fname _) `#',fname))
                         functions)))
     ,@body))
