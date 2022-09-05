;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.starpu-backend)

(defun blueprint-codelet (blueprint)
  (with-blueprint-info (blueprint)
    (starpu:make-codelet
     :name (intern (princ-to-string blueprint) #.*package*)
     :modes (append (make-list (length *dst-array-info*) :initial-element :w)
                    (make-list (length *src-array-info*) :initial-element :r))
     :cpu-func-0 (blueprint-cpu-func)
     :cuda-func-0 (blueprint-cuda-func))))

(defvar *foreign-gensym-counter-cell* (list 0))

(defun foreign-gensym (&optional (prefix "g"))
  (format nil "~A~7,'0D"
          prefix
          (atomics:atomic-incf
              (car *foreign-gensym-counter-cell*))))

(defun blueprint-cpu-func ()
  (let ((name (foreign-gensym "cpu_func")))
    (cffi:foreign-symbol-pointer
     name
     :library
     (starpu:load-foreign-code
      (with-output-to-string (stream)
        (write-blueprint-cpp name stream))
      :flags (list* "-O3" "-march=native" "-fPIC"
                    (starpu:pkg-config "starpu-1.3" "--cflags" "--libs"))))))

(defun blueprint-cuda-func ()
  (let ((name (foreign-gensym "cuda_func")))
    (cffi:foreign-symbol-pointer
     name
     :library
     (starpu:load-foreign-code
      (with-output-to-string (stream)
        (write-blueprint-cuda name stream))
      :compiler "nvcc"
      :language "cu"
      :flags (list* "-O3" "-arch=sm_50"
                    (starpu:pkg-config "starpu-1.3" "--cflags" "--libs"))))))
