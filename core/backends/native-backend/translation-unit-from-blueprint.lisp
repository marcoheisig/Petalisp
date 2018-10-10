;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From Blueprints to Translation Units

(defun translation-unit-from-blueprint (blueprint)
  (trivia:ematch (ucons:copy-utree blueprint)
    ((list ranges storage-types stores loads instructions)
     (let ((translation-unit
             (make-instance 'translation-unit
               :storage-types storage-types
               :initial-basic-block (basic-block-from-ranges ranges))))
       ;; Add instructions that compute the row-major-index for each load
       ;; and store.
       (break)
       ;; Now add all instructions to the suitable basic blocks.
       (loop for (stores operator loads) in instructions do
         (add-instruction translation-unit stores operator loads))))))

(defun basic-block-from-ranges (ranges)
  (flet ((convert (range successor)
           (let ((depth (if successor (1+ (depth successor)) 0)))
             (trivia:ematch range
               ((list :loop fixnum-p lb ub)
                (make-loop-block fixnum-p lb ub depth successor))
               ((list :reduction stores operator fixnum-p lb ub)
                (make-reduction-block stores operator fixnum-p lb ub depth successor))))))
    (make-basic-block
     (reduce #'convert ranges
             :from-end t
             :initial-value nil))))

;;; Place an instruction of the form (OUTPUTS OPERATOR . INPUTS) into the
;;; suitable basic block.
#+nil
(defun add-instruction (translation-unit stores operator loads)
  (let* ((actual-inputs
           (mapcar #'resolve-aliases inputs))
         (basic-block-index
           (loop for input in actual-inputs
                 maximize (basic-block-index input)))
         (basic-block
           (basic-block basic-block-index))
         (alias
           (find (cons operator inputs)
                 (instruction basic-block)
                 :key #'cdr
                 :test #'equal)))
    (cond ((not alias)
           (loop for output in outputs do
             (setf (basic-block-index output)
                   basic-block-index))
           (push (cons outputs (cons operator actual-inputs))
                 (instructions basic-block)))
          (alias
           (let ((alias-outputs (cdr alias)))
             (loop for output in outputs
                   for alias-output in alias-outputs do
                     (setf (alias-table output) alias-output)))))
    (values)))
