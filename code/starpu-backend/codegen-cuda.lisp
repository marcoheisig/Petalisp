;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.starpu-backend)

(defun write-blueprint-cuda (name stream)
  (when *emit-verbose-code*
    (format stream "#include <stdio.h>~%"))
  (format stream "#include <math.h>~%")
  (format stream "#include <starpu.h>~%")
  (format stream "extern \"C\" {~%")
  (format stream "void ~A (void *buffers[], void *args) {~%" name)
  (write-prologue name stream)
  (break "TODO")
  (write-epilogue name stream)
  (format stream "}~%")
  (format stream "}~%"))
