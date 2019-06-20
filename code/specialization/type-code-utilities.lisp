;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Array Types

(defun array-element-type-code (array)
  (macrolet ((array-element-type-code-expander ()
               (let* ((uaets (map 'vector #'upgraded-array-element-type +types+)))
                 `(typecase array
                    ,@(loop for type across (remove-duplicates uaets :test #'alexandria:type=)
                            for type-code from 0
                            collect `((array ,type) (type-code-from-type-specifier ',type)))
                    (t (type-code-from-type-specifier 't))))))
    (array-element-type-code-expander)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type Code Caching

(deftype type-cache (rank)
  `(simple-array t ,(loop repeat rank collect type-code-limit)))

(defun make-type-code-cache (rank fn)
  (let ((cache (make-array (loop repeat rank collect type-code-limit))))
    (labels ((rec (n type-codes)
               (if (= n 0)
                   (setf (apply #'aref cache type-codes)
                         (apply fn type-codes))
                   (loop for type-code below type-code-limit do
                           (rec (1- n) (cons type-code type-codes))))))
      (rec rank '()))
    cache))

(defmacro with-type-code-caching (type-codes &body body)
  (assert (every #'symbolp type-codes))
  (assert (null (intersection type-codes lambda-list-keywords)))
  (let ((n (length type-codes))
        (cache (gensym "CACHE")))
    `(let ((,cache (load-time-value
                    (make-type-code-cache ,n (lambda ,type-codes ,@body)))))
       (declare (type-code ,@type-codes)
                (type (type-cache ,n) ,cache)
                (optimize (speed 3)))
       (aref ,cache ,@type-codes))))

(defun type-code-union (type-code-1 type-code-2)
  (with-type-code-caching (type-code-1 type-code-2)
    (type-code-from-type-specifier
     `(or ,(type-specifier-from-type-code type-code-1)
          ,(type-specifier-from-type-code type-code-2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lists of Type Codes

(defun type-codes (&rest type-codes)
  "Behaves just like LIST, but is limited to elements that are type codes.
This restriction makes it possible to avoid consing in some cases."
  type-codes)

(define-compiler-macro type-codes (&whole form &rest type-codes)
  (trivia:match type-codes
    ((list a) `(type-codes-1 ,a))
    ((list a b) `(type-codes-2 ,a ,b))
    (_ form)))

(defun type-codes-1 (type-code)
  (with-type-code-caching (type-code)
    (list type-code)))

(defun type-codes-2 (type-code-1 type-code-2)
  (with-type-code-caching (type-code-1 type-code-2)
    (list type-code-1 type-code-2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reasoning About Type Codes

(defun empty-type-code-p (type-code)
  (declare (type-code type-code))
  (= type-code (type-code-from-type-specifier 'nil)))

(defun subtypep-mask (type-specifier &optional env)
  (loop for type across +types+
        for bit = 1 then (ash bit 1)
        sum (if (subtypep type type-specifier env) bit 0)))

(defmacro type-code-subtypecase (type-code &body clauses &environment env)
  (alexandria:with-gensyms (tc)
    `(let ((,tc (the type-code ,type-code)))
       (cond
         ,@(loop for (type-specifier . body) in clauses
                 collect `((logbitp ,tc ,(subtypep-mask type-specifier env))
                           ,@body))))))

(defun type-code-subtypep (type-code-1 type-code-2)
  (with-type-code-caching (type-code-1 type-code-2)
    (subtypep (type-specifier-from-type-code type-code-1)
              (type-specifier-from-type-code type-code-2))))
