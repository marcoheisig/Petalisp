;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defclass translation-unit ()
  ((%symbol-table :initarg :symbol-table :reader symbol-table)
   (%initial-basic-block :initarg :initial-basic-block :reader initial-basic-block)
   (%array-types :initarg :array-types :reader array-types)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Translation Unit Creation

(defun make-translation-unit (array-types)
  (let* ((lambda-list '(ranges arrays functions))
         (symbol-table (make-hash-table :test #'eq))
         (initial-basic-block
           (make-lambda-block
            :lambda-list lambda-list
            :declarations `((ignorable ,@lambda-list)
                            (type (and (vector t) (not simple-vector)) ranges)
                            (type (and (vector t) (not simple-vector)) arrays)
                            (type (and (vector t) (not simple-vector)) functions)))))
    (loop for symbol in lambda-list
          do (setf (gethash symbol symbol-table) initial-basic-block))
    (make-instance 'translation-unit
      :symbol-table symbol-table
      :initial-basic-block initial-basic-block
      :array-types array-types)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenient Accessors

(defvar *translation-unit*)

(define-symbol-macro *symbol-table*
    (symbol-table *translation-unit*))

(define-symbol-macro *initial-basic-block*
    (initial-basic-block *translation-unit*))

(defun defining-basic-block (symbol)
  (or (gethash symbol *symbol-table*)
      (error "Undefined symbol: ~S" symbol)))

(defun (setf defining-basic-block) (basic-block symbol)
  (setf (gethash symbol *symbol-table*)
        basic-block))

(defun index-symbol (n)
  (alexandria:format-symbol '#:petalisp.native-backend "INDEX-~D" n))

(defun array-symbol (n)
  (pseudo-eval
   0
   `(aref arrays ,n)
   (elt (array-types *translation-unit*) n)))

(defun function-symbol (n)
  (pseudo-eval 0 `(aref functions ,n)))

(defun start-symbol (n)
  (pseudo-eval 0 `(aref ranges ,(+ (* n 3) 0))))

(defun step-symbol (n)
  (pseudo-eval 0 `(aref ranges ,(+ (* n 3) 1))))

(defun end-symbol (n)
  (pseudo-eval 0 `(aref ranges ,(+ (* n 3) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pseudo Evaluation
;;;
;;; Evaluation determines the values of an expression.  Pseudo evaluation,
;;; on the other hand, determines the variable that will be bound to one of
;;; the values of an expression, once the kernel is evaluated.  The
;;; variables that are introduced by the pseudo evaluation are
;;; automatically added to the symbol table of the current translation
;;; unit.

(defun pseudo-eval (value-n form &optional (type 't))
  (etypecase form
    (symbol form)
    (number form)
    (cons
     (destructuring-bind (first . rest) form
       (let* ((basic-block *initial-basic-block*)
              (arguments (mapcar (lambda (arg) (pseudo-eval 0 arg)) rest)))
         ;; Find the innermost basic block on which FORM depends.
         (loop for argument in arguments do
           (when (symbolp argument)
             (let ((new-basic-block (defining-basic-block argument)))
               (when (dominates basic-block new-basic-block)
                 (setf basic-block new-basic-block)))))
         ;; Add the new form to the corresponding basic block and register
         ;; its value symbol in the symbol table.
         (let ((symbol (value-symbol
                        value-n
                        (if (eq type 't)
                            `(,first ,@arguments)
                            `(the ,type (,first ,@arguments)))
                        basic-block)))
           (setf (gethash symbol *symbol-table*) basic-block)
           symbol))))))
