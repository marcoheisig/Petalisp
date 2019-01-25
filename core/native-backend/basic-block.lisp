;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defgeneric value-symbol (value-n form basic-block))

(defgeneric instruction-symbols (form basic-block))

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
   ;; describing which symbols carry which values of the form.
   (%value-symbol-table :initarg :value-symbol-table :reader value-symbol-table))
  (:default-initargs :immediate-dominator nil
                     :successors '()
                     :instructions '()
                     :value-symbol-table (make-hash-table :test #'equal)))

(defun make-basic-block (&key immediate-dominator)
  (make-instance 'basic-block
    :immediate-dominator immediate-dominator))

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
        (let ((entry (assoc value-n alist)))
          (cond ((not present-p)
                 (push form instructions)
                 (new-symbol))
                ((not entry)
                 (new-symbol))
                (t (cdr entry))))))))

(defmethod instruction-symbols (instruction (basic-block basic-block))
  (let* ((alist (gethash instruction (value-symbol-table basic-block)))
         (values (loop for (value-n . nil) in alist maximize (1+ value-n))))
    (loop for value-n below values
          collect (cdr (assoc value-n alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conversion to S-Expressions

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
             for variables = (instruction-symbols instruction basic-block)
             collect (list variables instruction))
     ,(tail-form basic-block)))

(defmethod tail-form ((basic-block basic-block))
  (trivia:ematch (successors basic-block)
    ((list) '(values))
    ((list successor) (form successor))))
