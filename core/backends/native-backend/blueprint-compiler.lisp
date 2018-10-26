;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;; A vector of instructions.
(defvar *instructions*)

;;; A hash table, mapping from each symbol to its defining basic block.
(defvar *symbol-table*)

;;; A lambda block.
(defvar *initial-basic-block*)

(defun lambda-expression-from-blueprint (blueprint)
  (multiple-value-bind (ranges arrays *instructions* reductions)
      (parse-blueprint blueprint)
    (let* ((dim (length ranges))
           (*gensym-counter* 0)
           (*symbol-table* (make-hash-table :test #'eq))
           ;; Create the initial basic block.
           (*initial-basic-block*
             (let* ((lambda-list '(ranges arrays functions))
                    (basic-block (make-lambda-block :lambda-list lambda-list)))
               (loop for symbol in lambda-list
                     do (setf (gethash symbol *symbol-table*) basic-block))
               basic-block))
           (idom *initial-basic-block*)
           (reduction-symbols '())
           (reduction-stores '()))
      ;; Create one basic block for each loop or reduction.
      (loop for (size-bits step-bits type) in (reverse ranges)
            for index from 0 do
              (let ((var (index-symbol index))
                    (start (pseudo-eval-0 `(aref ranges ,(+ (* index 3) 0))))
                    (end (pseudo-eval-0 `(aref ranges ,(+ (* index 3) 2)))))
                (let ((bb (if (and (= index (1- dim))
                                   (not (null reductions)))
                              (make-reduction-block :immediate-dominator idom
                                                    :reduction-min start
                                                    :reduction-max end
                                                    :reductions reductions
                                                    :reduction-var var
                                                    :reduction-var-type type)
                              (make-loop-block :immediate-dominator idom
                                               :loop-start start
                                               :loop-step (pseudo-eval-0 `(aref ranges ,(+ (* index 3) 1)))
                                               :loop-end end
                                               :loop-var var
                                               :loop-var-type type))))
                  (setf (gethash var *symbol-table*) bb)
                  (setf (successors idom) (list bb))
                  (setf idom bb))))
      ;; Now translate all instructions of the blueprint.
      (loop for instruction across *instructions*
            for index from 0 do
              (ecase (car instruction)
                ((:call :load)
                 (update-instruction index))
                ((:store)
                 (pseudo-eval-0 (update-instruction index)))
                ((:reduce)
                 (trivia:match (update-instruction index)
                   ((or (list* 'funcall _ symbols)
                        (list* _ symbols))
                    (loop for symbol in symbols do
                      (push symbol reduction-symbols)))))
                ((:reduction-store)
                 (push instruction reduction-stores))))
      ;; Now generate the reduction bindings (if necessary)
      (unless (null reductions)
        (let ((epilogue (make-lambda-block :lambda-list reduction-symbols
                                           :immediate-dominator (immediate-dominator idom))))
          (setf (successors idom) (list epilogue))
          (loop for reduction-symbol in reduction-symbols do
            (setf (gethash reduction-symbol *symbol-table*) epilogue))
          (loop for (nil nil array-number . irefs) in reduction-stores
                for symbol in (reverse reduction-symbols) do
                  (pseudo-eval-0
                   `(store ,symbol
                           (aref arrays ,array-number)
                           ,(translate-row-major-index array-number irefs))))
          (setf (reduction-symbols idom) reduction-symbols)))
      ;; Done.
      (form *initial-basic-block*))))

(defun basic-block-or-die (symbol)
  (or (gethash symbol *symbol-table*)
      (error "Undefined symbol: ~S" symbol)))

(defun pseudo-eval (value-n form)
  (etypecase form
    (symbol form)
    (number form)
    (cons
     (let* ((basic-block *initial-basic-block*)
            (arguments (mapcar #'pseudo-eval-0 (rest form))))
       ;; Find the appropriate basic block.
       (loop for argument in arguments do
         (when (symbolp argument)
           (let ((new-basic-block (basic-block-or-die argument)))
             (when (dominates basic-block new-basic-block)
               (setf basic-block new-basic-block)))))
       (let ((symbol (value-symbol value-n (cons (first form) arguments) basic-block)))
         (setf (gethash symbol *symbol-table*) basic-block)
         symbol)))))

(defun pseudo-eval-0 (form)
  (pseudo-eval 0 form))

(defun update-instruction (index)
  (setf (aref *instructions* index)
        (translate-instruction (aref *instructions* index))))

(defun translate-instruction (instruction)
  (trivia:ematch instruction
    ((list* (or :call :reduce) operator arguments)
     (let ((rest (mapcar #'translate-argument arguments)))
       (etypecase operator
         (symbol `(,operator . ,rest))
         (integer `(funcall (aref functions ,operator) . ,rest)))))
    ((list* :load array-number irefs)
     `(row-major-aref
       (aref arrays ,array-number)
       ,(translate-row-major-index array-number irefs)))
    ((list* :store argument array-number irefs)
     `(store ,(translate-argument argument)
             (aref arrays ,array-number)
             ,(translate-row-major-index array-number irefs)))
    ((list* :iref index scale offset)
     `(+ (* ,(index-symbol index) ,scale) ,offset))))

(defun translate-argument (argument)
  (destructuring-bind (value-n instruction-number) argument
    (pseudo-eval value-n (aref *instructions* instruction-number))))

(defun translate-row-major-index (array-number irefs)
  (let ((quads (sort (loop for iref in irefs
                           for index from 0
                           collect (list* index iref))
                     #'>= :key #'second)))
    (reduce
     (lambda (expression quad)
       (destructuring-bind (stride-index index scale offset) quad
         (let ((stride (translate-stride array-number stride-index)))
           `(+ (+ ,expression (* ,stride ,offset))
               (* ,(index-symbol index) (* ,stride ,scale))))))
     quads
     :initial-value '0)))

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
         (reductions '()))
    (loop for instruction across instructions do
      (when (eq (car instruction) :reduce)
        (push instruction reductions)))
    (values ranges arrays instructions (nreverse reductions))))
