;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;; A vector that has as many entries as there are instructions in the
;;; blueprint.  Initially, each entry is an integer, denoting the number of
;;; values of that instruction that are used.  During translation, this
;;; integer is replaced with the list of variables that hold the values.
(defvar *instruction-values*)

(defun lambda-expression-from-blueprint (blueprint)
  (multiple-value-bind (ranges arrays instructions *instruction-values* reduction-spec)
      (parse-blueprint blueprint)
    (let ((form-builder (make-kernel-form-builder ranges reduction-spec)))
      (loop for instruction across instructions
            for index from 0 do
              (setf (aref *instruction-values* index)
                    (multiple-value-list
                     (add-form (translate-instruction instruction)
                               form-builder
                               (aref *instruction-values* index)))))
      (form form-builder))))

(defun translate-instruction (instruction)
  (trivia:ematch instruction
    ((list* (or :call :reduce) operator arguments)
     (let ((rest (mapcar #'translate-argument arguments)))
       (etypecase operator
         (symbol `(,operator . ,rest))
         (integer `(funcall (aref functions ,operator) . ,rest)))))
    ((list* :load array-number irefs)
     (translate-memory-reference array-number irefs))
    ((list* :store argument array-number irefs)
     `(setf ,(translate-memory-reference array-number irefs)
            ,(translate-argument argument)))
    ((list* :iref index scale offset)
     `(+ (* ,(index-symbol index) ,scale) ,offset))))

(defun translate-argument (argument)
  (destructuring-bind (value-n instruction-number) argument
    (nth value-n (aref *instruction-values* instruction-number))))

(defun translate-memory-reference (array-number irefs)
  (let ((quads (sort (loop for iref in irefs
                           for index from 0
                           collect (list* index iref))
                     #'< :key #'second)))
    `(row-major-aref
      (aref arrays ,array-number)
      ,(reduce
        (lambda (expression quad)
          (destructuring-bind (stride-index index scale offset) quad
            (let ((stride (translate-stride array-number stride-index)))
              `(+ (+ ,expression (* ,stride ,offset))
                  (* ,(index-symbol index) (* ,stride ,scale))))))
        quads
        :initial-value '0))))

(defun translate-stride (array-number axis)
  `(stride (aref arrays ,array-number) ,axis))

;;; Return as multiple values
;;;
;;; 1. A list of range specifications
;;;
;;; 2. A list of array types
;;;
;;; 3. A vector of instructions
;;;
;;; 4. A vector with the number of return values of each instruction
;;;
;;; 5. An (operator . arity) alist describing the reductions of the blueprint.
(defun parse-blueprint (blueprint)
  ;; This cries for a DESTRUCTURE-ULIST macro...
  (let* ((ulist blueprint)          (ranges (ucons:copy-utree (ucons:ucar ulist)))
         (ulist (ucons:ucdr ulist)) (arrays (ucons:copy-utree (ucons:ucar ulist)))
         (ulist (ucons:ucdr ulist)) (instructions (apply #'vector (ucons:copy-utree (ucons:ucar ulist))))
         (n-instructions (length instructions))
         (n-values (make-array n-instructions :initial-element 0))
         (reduction-spec '()))
    (flet ((register-argument (argument)
             (destructuring-bind (value-n instruction-number) argument
               (maxf (aref n-values instruction-number) (1+ value-n)))))
      (loop for instruction across instructions do
        (trivia:match instruction
          ((list* :call _ arguments)
           (mapc #'register-argument arguments))
          ((list* :store value _)
           (register-argument value))
          ((list* :reduce operator arguments)
           (push (cons operator (length arguments)) reduction-spec)
           (mapc #'register-argument arguments)))))
    (values ranges arrays instructions n-values (nreverse reduction-spec))))
