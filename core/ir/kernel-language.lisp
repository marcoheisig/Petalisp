;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir)

;;; The kernel language.  It's grammar is:
;;;
;;;            KERNEL := STANDARD-KERNEL || REDUCTION-KERNEL
;;;
;;;   STANDARD-KERNEL := (standard-kernel ITERATION-SPACE STATEMENT*)
;;;
;;;  REDUCTION-KERNEL := (reduction-kernel ITERATION-SPACE OPERATOR (OUTPUT*) STATEMENT*)
;;;
;;;         STATEMENT := (OPERATOR (INPUT*) (OUTPUT TYPE)*)
;;;
;;;            OUTPUT := BUFFER-REFERENCE || SYMBOL
;;;
;;;             INPUT := BUFFER-REFERENCE || SYMBOL
;;;
;;;  BUFFER-REFERENCE := (BUFFER TRANSFORMATION)
;;;
;;;   ITERATION-SPACE := An object of type PETALISP:SHAPE
;;;
;;;          OPERATOR := A function, or a symbol naming a function.
;;;
;;;              TYPE := A type specifier.
;;;
;;;            BUFFER := An object of type PETALISP-IR:BUFFER.
;;;
;;;    TRANSFORMATION := An object of type PETALISP:TRANSFORMATION.
;;;
;;;            SYMBOL := A symbol.
;;;
;;; The following constraints apply to expressions of the kernel language:
;;;
;;; 1.1 Any transformation of a buffer reference must be a mapping from the
;;;     iteration space of the kernel to the shape of the buffer.
;;;
;;; Then the following constraints apply to symbols that appear as inputs
;;; or outputs of statements:
;;;
;;; 2.1 Each symbol may only appear once as the output of a statement.
;;;
;;; 2.2 Each symbol may only appear as the input of a statement after it
;;;     has appeared as the output of a previous statement.
;;;
;;; 2.3 In a reduction kernel, all symbols returned by the function
;;;     REDUCTION-OUTPUT, when called with integers less than the number of
;;;     outputs of that kernel, must appear as an OUTPUT of a statement.
;;;     They must never be used as an input.
;;;
;;; 2.4 The symbol NIL is an exception to rules 2.1 and 2.2.  It may be
;;;     used as an output multiple times, but never as an input.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Kernel Language Creation

(defun reduction-output (n)
  (petalisp-memoization:with-vector-memoization (n)
    (intern
     (format nil "REDUCTION-OUTPUT-~D" n)
     :petalisp-ir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Kernel Language Verification

(defun verify-kernel-language (kernel-language)
  (trivia:match kernel-language
    ((list* 'standard-kernel iteration-space statements)
     (verify-iteration-space iteration-space :reduction nil)
     (verify-statements statements iteration-space 0))
    ((list* 'reduction-kernel operator outputs iteration-space statements)
     (let ((k (length outputs)))
       (verify-operator operator)
       (verify-iteration-space iteration-space :reduction t)
       (verify-statements statements iteration-space k)))
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
                 (error "Multiple definitions of the symbol ~S."
                        symbol))
               (when (member symbol desired-reduction-outputs)
                 (push symbol bound-reduction-outputs))
               (setf (gethash symbol environment) t)))
      ;; Now process the statements.
      (loop for statement in statements do
        (trivia:match statement
          ((list* operator inputs output-type-pairs)
           (verify-operator operator)
           (loop for input in inputs do
             (if (symbolp input)
                 (process-input-symbol input)
                 (verify-reference input iteration-space)))
           (loop for (output type) in output-type-pairs do
             (if (symbolp output)
                 (process-output-symbol output)
                 (verify-reference output iteration-space))))
          (t
           (error "~S is not a valid kernel language statement."
                  statement))))
      ;; Check that all reduction outputs have been assigned.
      (unless (null (cl:set-difference
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
       (error "The transformation ~S does not map from the iteration space ~
               ~S to the shape of the buffer ~S."
              transformation iteration-space buffer)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miscellaneous Utilities

(defun compute-kernel-inputs-and-outputs (kernel-language)
  (let ((all-inputs '())
        (all-outputs '()))
    (verify-kernel-language kernel-language)
    (flet ((scan-statements (statements)
             (loop for (nil inputs . output-type-pairs) in statements do
               (loop for input in inputs
                     when (consp input)
                       do (pushnew (car input) all-inputs))
               (loop for (output nil) in output-type-pairs
                     when (consp output)
                       do (pushnew (car output) all-outputs)))))
      (trivia:ematch kernel-language
        ((list* 'standard-kernel _ statements)
         (scan-statements statements))
        ((list* 'reduction-kernel _ outputs statements)
         (loop for output in outputs
               do (pushnew output all-outputs))
         (scan-statements statements))))
    (values all-inputs all-outputs)))
