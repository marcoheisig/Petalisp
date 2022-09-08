;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.starpu-backend)

(declaim (simple-vector *buffer-starpu-data-vector*))
(defvar *buffer-starpu-data-vector*)

(defun buffer-starpu-data (buffer)
  (svref *buffer-starpu-data-vector* (buffer-number buffer)))

(defun (setf buffer-starpu-data) (value buffer)
  (setf (svref *buffer-starpu-data-vector* (buffer-number buffer))
        value))

(defmacro with-starpu-data ((cstate (&rest results) (&rest arguments)) &body body)
  (let ((csym (gensym "CSTATE"))
        (rsyms (loop repeat (length results) collect (gensym "RESULT")))
        (asyms (loop repeat (length arguments) collect (gensym "ARGUMENT"))))
    `(let ((,csym ,cstate)
           ,@(mapcar #'list rsyms results)
           ,@(mapcar #'list asyms arguments))
       (petalisp.utilities:with-pinned-objects (,@rsyms ,@asyms)
         (call-with-starpu-data ,csym (list ,@rsyms) (list ,@asyms) (lambda () ,@body))))))

(defun call-with-starpu-data (cstate results arguments thunk)
  (let* ((program (cstate-program cstate))
         (allocations (cstate-allocations cstate))
         (result-buffers (program-root-buffers program))
         (argument-buffers (cstate-argument-buffers cstate))
         (*buffer-starpu-data-vector* (make-array (program-number-of-buffers program) :initial-element nil))
         ;; Check and complete the results.
         (results
           (loop for result in results
                 for result-buffer in result-buffers
                 collect
                 (if (null result)
                     (make-buffer-like-array result-buffer)
                     (ensure-array-buffer-compatibility result result-buffer)))))
    ;; Allocate result data.
    (loop for result in results
          for result-buffer in result-buffers
          do (setf (buffer-starpu-data result-buffer)
                   (make-displaced-starpu-data result)))
    ;; Check the arguments.
    (mapcar #'ensure-array-buffer-compatibility arguments argument-buffers)
    ;; Allocate argument data.
    (loop for argument in arguments
          for argument-buffer in argument-buffers
          do (setf (buffer-starpu-data argument-buffer)
                   (make-displaced-starpu-data argument)))
    ;; Convert constants into StarPU data.
    (loop for (buffer . lazy-array) in (program-leaf-alist program) do
      (unless (starpu:datap (buffer-starpu-data buffer))
        (let ((delayed-action (lazy-array-delayed-action lazy-array)))
          (etypecase delayed-action
            (delayed-array
             (setf (buffer-starpu-data buffer)
                   (make-displaced-starpu-data
                    (delayed-array-storage delayed-action))))
            (delayed-unknown
             (error "Reference to an unbound unknown: ~S" lazy-array))))))
    ;; Turn the remaining allocations into StarPU data.
    (loop for buffers in allocations do
      (let ((data (or (buffer-starpu-data (first buffers))
                      (make-starpu-data (first buffers)))))
        (dolist (buffer buffers)
          (setf (buffer-starpu-data buffer) data))))
    (unwind-protect (funcall thunk)
      ;; Free all StarPU data.
      (do-program-buffers (buffer program)
        (let ((data (svref *buffer-starpu-data-vector* (buffer-number buffer))))
          (when (starpu:datap data)
            (if (root-buffer-p buffer)
                (starpu:data-unregister data)
                (starpu:data-unregister-no-coherency data))))))
    ;; Done.
    (values-list results)))

(defun make-starpu-data (buffer)
  (let* ((shape (buffer-shape buffer))
         (ntype (buffer-ntype buffer))
         (size (ceiling (petalisp.type-inference:ntype-bits ntype) 8)))
    (trivia:ematch (shape-ranges shape)
      ((list)
       (starpu:make-vector :nx 1 :size size))
      ((list (range a))
       (starpu:make-vector :nx a :size size))
      ((list (range a) (range b))
       (starpu:make-matrix :ny a :nx b :size size))
      ((list (range a) (range b) (range c))
       (starpu:make-block :nz a :ny b :nx c :size size))
      ((list* (range a) (range b) (range c) _)
       (starpu:make-block :nz a :ny b :nx c :size size)))))

(defun make-displaced-starpu-data (array)
  (case (array-rank array)
    (0 (starpu:make-displaced-vector array))
    (1 (starpu:make-displaced-vector array))
    (2 (starpu:make-displaced-matrix array))
    (3 (starpu:make-displaced-block array))
    (otherwise
     (starpu:make-displaced-block array))))
