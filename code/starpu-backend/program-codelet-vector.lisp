;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.starpu-backend)

(defparameter *kernel-scaling-threshold* 3)
(defparameter *kernel-offset-threshold* 2)

(defun program-codelet-vector (program &optional cache)
  (declare (petalisp.ir:program program)
           (type (or null hash-table) cache))
  (let* ((blueprint-vector (program-blueprint-vector program))
         (codelet-vector
           (map 'vector
                 (lambda (blueprint)
                   (and cache (gethash blueprint cache)))
                 blueprint-vector))
         (cpu-func-vector (cpu-func-vector blueprint-vector codelet-vector))
         (cuda-func-vector (cuda-func-vector blueprint-vector codelet-vector)))
    (loop for index below (length codelet-vector) do
      (symbol-macrolet ((blueprint (svref blueprint-vector index))
                        (codelet (svref codelet-vector index))
                        (cpu-func (svref cpu-func-vector index))
                        (cuda-func (svref cuda-func-vector index)))
        (when (not codelet)
          (let ((value
                  (starpu:make-codelet
                   :name (intern (princ-to-string blueprint) #.*package*)
                   :modes (blueprint-modes blueprint)
                   :cpu-func-0 cpu-func
                   ;:cuda-func-0 cuda-func
                   )))
            (setf codelet value)
            (when cache
              (setf (gethash blueprint cache) value))))))
    codelet-vector))

(defun blueprint-modes (blueprint)
  (trivia:ematch blueprint
    ((ucons:ulist* _ dst-info src-info _)
     (append (make-list (ucons:ulength dst-info) :initial-element :w)
             (make-list (ucons:ulength src-info) :initial-element :r)))))

(defun program-blueprint-vector (program)
  (let ((vector (make-array (petalisp.ir:program-number-of-kernels program)
                            :initial-element nil)))
    (do-program-kernels (kernel program vector)
      (setf (svref vector (kernel-number kernel))
            (kernel-blueprint
             kernel
             :scaling-threshold *kernel-scaling-threshold*
             :offset-threshold *kernel-offset-threshold*)))))

(defun cpu-func-vector (blueprint-vector codelet-vector)
  (let* ((stream (make-string-output-stream))
         (names
           (loop for blueprint across blueprint-vector
                 for codelet across codelet-vector
                 when (not codelet)
                   collect
                   (let ((name (foreign-gensym "cpu_func")))
                     (with-blueprint-info (blueprint)
                       (write-blueprint-cpp name stream))
                     name)))
         (library
           (unless (null names)
             (starpu:load-foreign-code
              (get-output-stream-string stream)
              :flags (list* "-O3" "-march=native" "-fPIC"
                            (starpu:pkg-config "starpu-1.3" "--cflags" "--libs"))))))
    (map 'vector
          (lambda (codelet)
            (when (not codelet)
              (cffi:foreign-symbol-pointer (pop names) :library library)))
          codelet-vector)))

(defun cuda-func-vector (blueprint-vector codelet-vector)
  (let* ((stream (make-string-output-stream))
         (names
           (loop for blueprint across blueprint-vector
                 for codelet across codelet-vector
                 when (not codelet)
                   collect
                   (let ((name (foreign-gensym "cuda_func")))
                     (with-blueprint-info (blueprint)
                       (write-blueprint-cuda name stream))
                     name)))
         (library
           (unless (null names)
             (starpu:load-foreign-code
              (get-output-stream-string stream)
              :compiler "nvcc"
              :language "cu"
              :flags (list* "-O3" "-arch=sm_50"
                            (starpu:pkg-config "starpu-1.3" "--cflags" "--libs"))))))
    (map 'vector
          (lambda (codelet)
            (when (not codelet)
              (cffi:foreign-symbol-pointer (pop names) :library library)))
          codelet-vector)))

(defvar *foreign-gensym-counter-cell* (list 0))

(defun foreign-gensym (&optional (prefix "g"))
  (format nil "~A~7,'0D"
          prefix
          (atomics:atomic-incf
              (car *foreign-gensym-counter-cell*))))
