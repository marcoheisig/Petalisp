;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(uiop:define-package :petalisp/utilities/optimization
  (:use :closer-common-lisp :alexandria :trivia)
  (:export
   #:with-unsafe-optimizations
   #:with-unsafe-optimizations*
   #:dx-let
   #:dx-let*
   #:dx-flet))

(in-package :petalisp/utilities/optimization)

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
               ,@(mapcar (lambda-ematch
                           ((list var _) var)
                           (var var))
                         bindings)))
     ,@body))

(defmacro dx-let* (bindings &body body)
  "Like LET*, but declare every variable to have dynamic extent."
  `(let* ,bindings
     (declare (dynamic-extent
               ,@(mapcar (lambda-ematch
                           ((list var _) var)
                           ((list var) var)
                           (var var))
                         bindings)))
     ,@body))

(defmacro dx-flet (functions &body body)
  "Like FLET, but declare every function to have dynamic extent."
  `(flet ,functions
     (declare (dynamic-extent
               ,@(mapcar (lambda-ematch ((list* fname _) `#',fname))
                         functions)))
     ,@body))
