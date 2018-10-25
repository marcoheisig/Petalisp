;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

(defgeneric value-symbol (value-n form basic-block))

(defgeneric instruction-symbols (form basic-block))

(defgeneric form (basic-block))

(defclass basic-block ()
  (;; A (possibly empty) list containing all dominators of the basic block.
   ;; The first element of this list is the immediate dominator.
   (%dominators :initarg :dominators :reader dominators)
   ;; A list of lists of the form (variables form).  The instructions
   ;; are stored in reverse order of execution, because we typically insert
   ;; at the end.
   (%instructions :initarg :instructions :accessor instructions)
   ;; A hash table, mapping from forms to (integer . symbol) alists,
   ;; describing which symbols carry which values of the form.
   (%value-symbol-table :initarg :value-symbol-table :reader value-symbol-table)))

(defun make-basic-block (&optional dominator)
  (make-instance 'basic-block
    :dominators (if dominator (list* dominator (dominators dominator)) '())
    :instructions '()
    :value-symbol-table (make-hash-table :test #'equal)))

(defun dominates (a b)
  (member a (dominators b)))

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
;;; Basic Blocks to S-Expressions

(defmacro bind (variables instruction body)
  (let ((ignore '()))
    (flet ((frob (variable)
             (if (null variable)
                 (let ((symbol (gensym)))
                   (push symbol ignore)
                   symbol)
                 variable)))
      `(multiple-value-bind ,(mapcar #'frob variables) ,instruction
           (declare (ignore ,@ignore))
         ,body))))

(defmacro basic-block (&body body)
  (trivia:match body
    ((list) `(values))
    ((list* (list variables instruction) rest)
     `(bind ,variables ,instruction (basic-block . ,rest))))
  )

(defmethod form ((basic-block basic-block))
  `(basic-block
     ,@(loop for instruction in (reverse (instructions basic-block))
             for variables = (instruction-symbols instruction basic-block)
             collect (list variables instruction))))
