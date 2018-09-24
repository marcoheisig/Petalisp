;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir)

;;; The kernel language.  It's grammar is:
;;;
;;;           KERNEL := STANDARD-KERNEL || REDUCTION-KERNEL
;;;
;;;  STANDARD-KERNEL := (standard-kernel ITERATION-SPACE STATEMENT*)
;;;
;;; REDUCTION-KERNEL := (reduction-kernel OPERATOR K ITERATION-SPACE STATEMENT*)
;;;
;;;        STATEMENT := (OPERATOR (INPUT*) (OUTPUT*))
;;;
;;;           OUTPUT := (BUFFER TRANSFORMATION) || SYMBOL
;;;
;;;            INPUT := (BUFFER TRANSFORMATION) || SYMBOL
;;;
;;;  ITERATION-SPACE := An object of type PETALISP:SHAPE
;;;
;;;         OPERATOR := A function, or a symbol naming a bound function.
;;;
;;;                K := The number of output values of the reduction's operator.
;;;
;;;           BUFFER := An object of type PETALISP-IR:BUFFER.
;;;
;;;   TRANSFORMATION := An object of type PETALISP:TRANSFORMATION.
;;;
;;;           SYMBOL := A symbol.
;;;
;;; The following constraints apply to symbols that appear as inputs or
;;; outputs of statements:
;;;
;;; 1. Each symbol may only appear once as the output of a statement.
;;;
;;; 2. Each symbol may only appear as the input of a statement after it has
;;;    appeared as the output of a previous statement.
;;;
;;; 3. The symbols returned by the functions LEFT-REDUCTION-INPUT or
;;;    RIGHT-REDUCTION-INPUT, when called with an integer less than K, are
;;;    an exception to rules 1 and 2.  They are treated as if they had
;;;    already appeared as an output of a statement.
;;;
;;; 4. All symbols returned by the functions REDUCTION-OUTPUT, when called
;;;    with integers less than K, must appear as an OUTPUT of a statement,
;;;    and never as an input.
;;;
;;; Furthermore, the transformation of an input or output must be a mapping
;;; from the iteration space of the kernel to the shape of the buffer.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Kernel Language Creation

(defun left-reduction-input (n)
  (petalisp-memoization:with-vector-memoization (n)
    (symbolicate "LEFT-REDUCTION-INPUT-" n)))

(defun right-reduction-input (n)
  (petalisp-memoization:with-vector-memoization (n)
    (symbolicate "RIGHT-REDUCTION-INPUT-" n)))

(defun reduction-output (n)
  (petalisp-memoization:with-vector-memoization (n)
    (symbolicate "REDUCTION-OUTPUT-" n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Kernel Language Verification

(defun verify-kernel-language (kernel-language)
  (trivia:match kernel-language
    ((list* 'standard-kernel iteration-space statements)
     (verify-iteration-space iteration-space :reduction nil)
     (verify-statements statements iteration-space 0))
    ((list* 'reduction-kernel operator k iteration-space statements)
     (verify-operator operator)
     (unless (typep k '(integer 1 #.multiple-values-limit))
       (error "~S is not a valid reduction arity specifier." k))
     (verify-iteration-space iteration-space :reduction t)
     (verify-statements statements iteration-space k))
    (t
     (error "~S is not a valid kernel language expression."
            kernel-language))))

(defun verify-statements (statements iteration-space k)
  (let ((environment (make-hash-table))
        (bound-reduction-outputs '())
        (desired-reduction-outputs
          (loop for i below k
                collect (reduction-output i))))
    (labels ((process-input-symbol (symbol)
               (when (member symbol desired-reduction-outputs)
                 (error "The symbol ~S must not appear as an input."
                        symbol))
               (unless (gethash symbol environment)
                 (error "The symbol ~S has not yet been defined."
                        symbol)))
             (process-output-symbol (symbol)
               (when (gethash symbol environment)
                 (error "Multiple assignments to the symbol ~S."
                        symbol))
               (setf (gethash symbol environment) t)))
      ;; Add all reduction input symbols to the environment.
      (loop for i below k do
        (process-output-symbol (left-reduction-input i))
        (process-output-symbol (right-reduction-input i)))
      ;; Now process the statements.
      (loop for statement in statements do
        (trivia:match statement
          ((list operator inputs outputs)
           (verify-operator operator)
           (loop for input in inputs do
             (if (symbolp input)
                 (process-input-symbol input)
                 (verify-reference input iteration-space)))
           (loop for output in outputs do
             (if (symbolp output)
                 (process-output-symbol output)
                 (verify-reference output iteration-space))))
          (t
           (error "~S is not a valid kernel language statement."
                  statement))))
      ;; Check that all reduction outputs have been assigned.
      (unless (null (set-difference
                     desired-reduction-outputs
                     bound-reduction-outputs))
        (error "Not all reduction outputs have been assigned.")))))

(defun verify-reference (reference iteration-space)
  (trivia:match reference
    ((list buffer transformation)
     (unless (and (typep buffer 'petalisp-ir:buffer)
                  (typep transformation 'petalisp:transformation))
       (trivia.fail:fail))
     (unless (and (= (input-dimension transformation)
                     (dimension iteration-space))
                  (= (output-dimension transformation)
                     (dimension (shape buffer)))
                  (set-subsetp
                   (transform iteration-space transformation)
                   (shape buffer)))
       (error "The transformation ~S does not map from the iteration space~
               ~S to the shape of the buffer ~S."
              iteration-space iteration-space buffer)))
    (t
     (error "~S is not a valid kernel language reference."
            reference))))

(defun verify-operator (operator)
  (unless (functionp operator)
    (unless (and (symbolp operator)
                 (fboundp operator))
      (error "~S is not a valid kernel language operator."
             operator))))

(defun verify-iteration-space (iteration-space &key reduction)
  (unless (typep iteration-space 'petalisp:shape)
    (error "~S is not a valid iteration space."
           iteration-space))
  (when (and reduction
             (not (plusp (petalisp:dimension iteration-space))))
    (error "A reduction iteration space must at least have dimension one, found ~S."
           iteration-space)))
