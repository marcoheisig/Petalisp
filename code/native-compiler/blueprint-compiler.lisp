;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.blueprint-compiler)

;;; A vector of instructions.  With this vector, we can look up the
;;; instruction corresponding to an instruction number in constant time.
(defvar *instructions*)

(defun translate-blueprint (blueprint)
  (multiple-value-bind (range-info array-info instructions)
      (petalisp.ir:parse-kernel-blueprint blueprint)
    (let ((*instructions* (coerce instructions 'simple-vector))
          (*translation-unit* (make-translation-unit array-info))
          ;; Resetting the gensym counter makes the generated code more
          ;; legible.
          (*gensym-counter* 0)
          (innermost-block nil))
      ;; Add loop blocks.
      (let ((immediate-dominator *initial-basic-block*))
        (loop for info in range-info
              for index from 0 do
                (let ((loop-block (add-loop-block info index immediate-dominator)))
                  (setf (successors immediate-dominator)
                        (list loop-block))
                  (setf immediate-dominator loop-block)))
        (setf innermost-block immediate-dominator))
      ;; Now translate and pseudo-evaluate all store instructions and their
      ;; dependencies.
      (loop for instruction across *instructions*
            for index from 0 do
              (when (eq (car instruction) :store)
                (pseudo-eval 0 (instruction index))))
      ;; Done.
      (form *initial-basic-block*))))

(defun add-loop-block (range-info index immediate-dominator)
  (let* ((loop-index (index-symbol index))
         (start (start-symbol index))
         (step (step-symbol index))
         (end (end-symbol index)))
    (setf (defining-basic-block loop-index)
          (make-loop-block
           :start start
           :step step
           :end end
           :var loop-index
           :info range-info
           :immediate-dominator immediate-dominator))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Translation

(defun instruction (instruction-number)
  (let ((form (aref *instructions* instruction-number)))
    (if (not (keywordp (first form)))
        form
        (setf (aref *instructions* instruction-number)
              (translate-instruction form)))))

(defun translate-instruction (instruction)
  (trivia:ematch instruction
    ((list* :call operator arguments)
     (let ((rest (mapcar #'pseudo-eval-argument arguments)))
       (etypecase operator
         (symbol `(,operator . ,rest))
         (integer `(funcall ,(function-symbol operator) . ,rest)))))
    ((list* :load array-number irefs)
     `(row-major-aref
       ,(array-symbol array-number)
       ,(translate-row-major-index array-number irefs)))
    ((list* :store argument array-number irefs)
     `(store ,(pseudo-eval-argument argument)
             ,(array-symbol array-number)
             ,(translate-row-major-index array-number irefs)))
    ((list :iref index scale offset)
     (if (null index)
         `(identity ,offset)
         `(identity ,(i+ (i* (index-symbol index) scale) offset))))
    ((list* :rref symbols)
     `(values ,@symbols))))

(defun pseudo-eval-argument (argument)
  (destructuring-bind (value-n instruction-number) argument
    (pseudo-eval value-n (instruction instruction-number))))

(defun translate-row-major-index (array-number irefs)
  (let* ((quads (sort (loop for (index scale offset) in irefs
                            for axis from 0
                            collect
                            (if (null index)
                                (list axis -42 scale offset)
                                (list axis index scale offset)))
                      #'< :key #'second))
         (array-rank (length irefs)))
    (reduce
     (lambda (expression quad)
       (destructuring-bind (axis index scale offset) quad
         (let ((stride (translate-stride array-number array-rank axis)))
           (i+ (i+ expression (i* stride offset))
               (i* (if (= index -42) 0 (index-symbol index))
                   (i* stride scale))))))
     quads
     :initial-value '0)))

(defun translate-stride (array-number array-rank axis)
  (if (= axis (1- array-rank))
      1
      (i* `(array-dimension ,(array-symbol array-number) ,(1+ axis))
          (translate-stride array-number array-rank (1+ axis)))))
