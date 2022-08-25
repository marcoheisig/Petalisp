;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.starpu-backend)

(defmethod backend-evaluator
    ((starpu-backend starpu-backend)
     (unknowns list)
     (lazy-arrays list))
  (funcall (evaluator-generator (length unknowns) (length lazy-arrays))
           (make-cstate starpu-backend unknowns lazy-arrays)))

(let ((cache (make-hash-table)))
  (defun evaluator-generator (number-of-results number-of-arguments)
    (alexandria:ensure-gethash
     number-of-arguments
     (alexandria:ensure-gethash number-of-results cache (make-hash-table))
     (let ((results (result-variables number-of-results))
           (arguments (argument-variables number-of-arguments)))
       (compile
        nil
        `(lambda (cstate)
           (lambda (,@results ,@arguments)
             (let ((dstate (make-dstate cstate)))
               (unwind-protect (evaluate cstate dstate ,@arguments)
                 (finalize cstate dstate ,@results))))))))))

(defun generate-variable (prefix integer)
  (intern
   (with-output-to-string (stream)
     (loop for char across (string prefix) do
       (write-char char stream))
     (format stream "~D" integer))
   #.*package*))

(defun result-variables (n)
  (loop for i below n collect (generate-variable "DST" i)))

(defun argument-variables (n)
  (loop for i below n collect (generate-variable "SRC" i)))
