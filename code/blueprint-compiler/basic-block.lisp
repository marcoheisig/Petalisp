;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.blueprint-compiler)

(defgeneric value-symbol (value-n form basic-block))

(defgeneric form (basic-block))

(defgeneric tail-form (basic-block))

(defclass basic-block ()
  (;; Another basic block that immediately dominates this one, or NIL.
   (%immediate-dominator :initarg :immediate-dominator :reader immediate-dominator)
   ;; A (possibly empty) list of basic blocks.
   (%successors :initarg :successors :accessor successors)
   ;; A list of lists of the form (variables form).  The instructions are
   ;; stored in reverse order of execution, because we typically insert at
   ;; the end.
   (%instructions :initarg :instructions :accessor instructions)
   ;; A hash table, mapping from forms to (integer . symbol) alists,
   ;; describing which symbol carries which value of the form.
   (%value-symbol-table :initarg :value-symbol-table :reader value-symbol-table))
  (:default-initargs :immediate-dominator nil
                     :successors '()
                     :instructions '()
                     :value-symbol-table (make-hash-table :test #'equal)))

(defun dominates (a b)
  (let ((idom (immediate-dominator b)))
    (if (not idom)
        nil
        (or (eq a idom)
            (dominates a idom)))))

(defmethod value-symbol (value-n form (basic-block basic-block))
  (with-accessors ((instructions instructions)
                   (value-symbol-table value-symbol-table)) basic-block
    (flet ((new-symbol ()
             (let ((symbol (gensym)))
               (push (cons value-n symbol)
                     (gethash form value-symbol-table '()))
               symbol)))
      (multiple-value-bind (alist present-p)
          (gethash form value-symbol-table)
        (if (not present-p)
            (progn (push form instructions)
                   (new-symbol))
            (let ((entry (assoc value-n alist)))
              (if (null entry)
                  (new-symbol)
                  (cdr entry))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic Block Translation

(defmacro bind (variables instruction body)
  (flet ((frob (variable)
           (if (null variable) (gensym) variable)))
    (let ((symbols (mapcar #'frob variables)))
      `(multiple-value-bind ,symbols ,instruction
         (declare (ignorable ,@symbols))
         ,body))))

(defmacro basic-block (&body body)
  (trivia:match body
    ((list tail) tail)
    ((list* (list variables instruction) rest)
     `(bind ,variables ,instruction (basic-block . ,rest)))))

(defmethod form ((basic-block basic-block))
  `(basic-block
     ,@(loop for instruction in (reverse (instructions basic-block))
             for variables = (instruction-value-symbols instruction basic-block)
             collect (list variables instruction))
     ,(tail-form basic-block)))

(defmethod tail-form ((basic-block basic-block))
  (trivia:ematch (successors basic-block)
    ((list) '(values))
    ((list successor) (form successor))))

(defun instruction-value-symbols (instruction basic-block)
  (let* ((alist (gethash instruction (value-symbol-table basic-block)))
         (n-values (1+ (loop for (value-n . nil) in alist maximize value-n))))
    (loop for value-n below n-values
          collect (cdr (assoc value-n alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lambda Blocks

(defclass lambda-block (basic-block)
  ((%lambda-list :initarg :lambda-list :reader lambda-list)
   (%declarations :initarg :declarations :reader declarations)))

(defun make-lambda-block (&key lambda-list immediate-dominator declarations)
  (make-instance 'lambda-block
    :lambda-list lambda-list
    :immediate-dominator immediate-dominator
    :declarations declarations))

(defmethod form ((lambda-block lambda-block))
  (let ((symbols (lambda-list lambda-block)))
    `(lambda ,symbols
       (declare ,@(declarations lambda-block))
       (with-unsafe-optimizations
         ,(call-next-method)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Loop Blocks

(defclass loop-block (basic-block)
  ((%start :initarg :start :reader loop-start)
   (%step :initarg :step :reader loop-step)
   (%end :initarg :end :reader loop-end)
   (%var :initarg :var :reader loop-var)
   (%info :initarg :info :reader loop-info)))

(defun make-loop-block (&rest args)
  (apply #'make-instance 'loop-block args))

(defmethod form ((loop-block loop-block))
  (with-accessors ((start loop-start)
                   (step loop-step)
                   (end loop-end)
                   (var loop-var)
                   (info loop-info)) loop-block
    (ecase info
      (:single
       `(let ((,var (the fixnum ,start)))
          (declare (ignorable ,var))
          ,(call-next-method)))
      (:contiguous
       `(loop for ,var fixnum from ,start to ,end
              do ,(call-next-method)))
      (:strided
       `(loop for ,var fixnum from ,start by ,step to ,end
              do ,(call-next-method))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Progn Blocks

(defclass progn-block (basic-block)
  ((%tail-form :initarg :tail-form :accessor tail-form)))

(defun make-progn-block (&key tail-form immediate-dominator)
  (make-instance 'progn-block
    :tail-form tail-form
    :immediate-dominator immediate-dominator))
