;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;; A vector of instructions.
(defvar *instructions*)

;;; A hash table, mapping from each symbol to its defining basic block.
(defvar *symbol-table*)

;;; A vector.
(defvar *array-symbols*)

(defvar *initial-basic-block*)

(defvar *final-basic-block*)

(defun lambda-expression-from-blueprint (blueprint)
  (multiple-value-bind (ranges array-types *instructions* reductions)
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
           (*final-basic-block* *initial-basic-block*)
           (reduction-block nil))
      (loop for loop-range in (reverse (if (null reductions) ranges (cdr ranges)))
            for index downfrom (1- dim) do
              (push-loop-block loop-range index))
      ;; If necessary, add a reduction block.
      (unless (null reductions)
        (destructuring-bind (size-bits step-bits type) (first ranges)
          (let* ((index 0)
                 (var (index-symbol index))
                 (start (pseudo-eval-0 `(aref ranges ,(+ (* index 3) 0))))
                 (step (pseudo-eval-0 `(aref ranges ,(+ (* index 3) 1))))
                 (end (pseudo-eval-0 `(aref ranges ,(+ (* index 3) 2)))))
            (setf reduction-block
                  (make-reduction-block :immediate-dominator *final-basic-block*
                                        :reduction-start start
                                        :reduction-step step
                                        :reduction-end end
                                        :reductions reductions
                                        :reduction-var var
                                        :reduction-var-type type))
            (setf (gethash var *symbol-table*) reduction-block))))
      ;; Translate all load, store and call instructions and add them to
      ;; the relevant basic blocks.
      (let ((reduce-counter 0)
            (reduction-symbols '())
            (*array-symbols*
             (let* ((n-arrays (length array-types))
                    (array-symbols (make-array n-arrays)))
               (loop for index below n-arrays
                     for array-type in array-types do
                 (setf (aref array-symbols index)
                       (pseudo-eval 0 `(aref arrays ,index) array-type)))
               array-symbols)))
        (loop for instruction across *instructions*
              for index from 0 do
                (case (car instruction)
                  ((:call :load :iref)
                   (update-instruction index))
                  ((:store)
                   (pseudo-eval-0 (update-instruction index)))
                  ((:reduce)
                   (setf (aref *instructions* index) reduce-counter)
                   (loop for (value-n inst) in (cddr instruction) do
                     (push (pseudo-eval value-n (aref *instructions* inst)) reduction-symbols)
                     (incf reduce-counter)))))
        (when reduction-block
          (setf (reduction-symbols reduction-block)
                (nreverse reduction-symbols))
          (let ((reduction-thunk-symbol
                  (value-symbol 0 (form reduction-block) *final-basic-block*)))
            (setf (gethash reduction-thunk-symbol *symbol-table*) *final-basic-block*)
            (loop for instruction across *instructions*
                  for index from 0 do
                    (trivia:match instruction
                      ((list* :reduction-store (list value-n index) array-number irefs)
                       (value-symbol
                        0
                        `(store ,(pseudo-eval
                                  (+ value-n (aref *instructions* index))
                                  `(funcall ,reduction-thunk-symbol))
                                ,(aref *array-symbols* array-number)
                                ,(pseudo-eval-0
                                  (translate-row-major-index array-number irefs)))
                        *final-basic-block*)))))))
      ;; Done.
      (form *initial-basic-block*))))

(defun push-loop-block (range index)
  (destructuring-bind (size-bits step-bits type) range
    (let ((var (index-symbol index))
          (start (pseudo-eval-0 `(aref ranges ,(+ (* index 3) 0))))
          (step (pseudo-eval-0 `(aref ranges ,(+ (* index 3) 1))))
          (end (pseudo-eval-0 `(aref ranges ,(+ (* index 3) 2)))))
      (let ((loop-block (make-loop-block :immediate-dominator *final-basic-block*
                                         :loop-start start
                                         :loop-step step
                                         :loop-end end
                                         :loop-var var
                                         :loop-var-type type)))
        (setf (gethash var *symbol-table*) loop-block)
        (setf (successors *final-basic-block*) (list loop-block))
        (setf *final-basic-block* loop-block)))))

(defun basic-block-or-die (symbol)
  (or (gethash symbol *symbol-table*)
      (error "Undefined symbol: ~S" symbol)))

(defun pseudo-eval (value-n form &optional (type 't))
  (etypecase form
    (symbol form)
    (number form)
    (cons
     (destructuring-bind (first . rest) form
       (let* ((basic-block *initial-basic-block*)
              (arguments (mapcar #'pseudo-eval-0 rest)))
         ;; Find the appropriate basic block.
         (loop for argument in arguments do
           (when (symbolp argument)
             (let ((new-basic-block (basic-block-or-die argument)))
               (when (dominates basic-block new-basic-block)
                 (setf basic-block new-basic-block)))))
         (let ((symbol (value-symbol
                        value-n
                        (if (eq type 't)
                            `(,first ,@arguments)
                            `(the ,type (,first ,@arguments)))
                        basic-block)))
           (setf (gethash symbol *symbol-table*) basic-block)
           symbol))))))

(defun pseudo-eval-0 (form)
  (pseudo-eval 0 form))

(defun update-instruction (index)
  (setf (aref *instructions* index)
        (translate-instruction (aref *instructions* index))))

(defun translate-instruction (instruction)
  (trivia:ematch instruction
    ((list* :call operator arguments)
     (let ((rest (mapcar #'translate-argument arguments)))
       (etypecase operator
         (symbol `(,operator . ,rest))
         (integer `(funcall (aref functions ,operator) . ,rest)))))
    ((list* :load array-number irefs)
     `(row-major-aref
       ,(aref *array-symbols* array-number)
       ,(translate-row-major-index array-number irefs)))
    ((list* :store argument array-number irefs)
     `(store ,(translate-argument argument)
             ,(aref *array-symbols* array-number)
             ,(translate-row-major-index array-number irefs)))
    ((list :iref index scale offset)
     (if (null index)
         `(identity ,offset)
         `(identity ,(i+ (i* (index-symbol index) scale) offset))))))

(defun translate-argument (argument)
  (destructuring-bind (value-n instruction-number) argument
    (pseudo-eval value-n (aref *instructions* instruction-number))))

(defun translate-row-major-index (array-number irefs)
  (let* ((quads (sort (loop for (index scale offset) in irefs
                            for axis from 0
                            unless (null index)
                              collect (list axis index scale offset))
                      #'< :key #'second))
         (array-rank (length irefs)))
    (reduce
     (lambda (expression quad)
       (destructuring-bind (axis index scale offset) quad
         (let ((stride (translate-stride array-number array-rank axis)))
           (i+ (i+ expression (i* stride offset))
               (i* (index-symbol index)
                   (i* stride scale))))))
     quads
     :initial-value '0)))

(defun translate-stride (array-number array-rank axis)
  (if (= axis (1- array-rank))
      1
      (i* `(array-dimension ,(aref *array-symbols* array-number) ,(1+ axis))
          (translate-stride array-number array-rank (1+ axis)))))

;;; Return as multiple values
;;;
;;; 1. A list of range specifications.
;;;
;;; 2. A list of array types.
;;;
;;; 3. A vector of instructions.
;;;
;;; 5. A list of all reduce instructions.
(defun parse-blueprint (blueprint)
  ;; This cries for a DESTRUCTURE-ULIST macro...
  (let* ((ulist blueprint)          (ranges (ucons:copy-utree (ucons:ucar ulist)))
         (ulist (ucons:ucdr ulist)) (arrays (ucons:copy-utree (ucons:ucar ulist)))
         (ulist (ucons:ucdr ulist)) (instructions (apply #'vector (ucons:copy-utree (ucons:ucar ulist))))
         (reductions '()))
    (loop for instruction across instructions do
      (when (eq (car instruction) :reduce)
        (push instruction reductions)))
    (values ranges arrays instructions (nreverse reductions))))
