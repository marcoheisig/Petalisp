;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Array Ntypes

(declaim (inline array-element-ntype))
(defun array-element-ntype (array)
  (macrolet ((body ()
               (let ((ntypes (remove-duplicates
                              *ntypes*
                              :test #'alexandria:type=
                              :key (alexandria:compose
                                    #'upgraded-array-element-type
                                    #'%ntype-type-specifier))))
                 `(typecase array
                    ,@(loop for ntype across ntypes
                            collect
                            `((array ,(%ntype-type-specifier ntype)) ',ntype))
                    (t ',(ntype 't))))))
    (body)))
(declaim (notinline array-element-ntype))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reasoning About Ntypes

(defun empty-ntype-p (ntype)
  (and (%ntypep ntype)
       (ntype= ntype (ntype 'nil))))

(defun universal-ntypep-p (ntype)
  (and (%ntypep ntype)
       (ntype= ntype (ntype 't))))

(defun eql-ntype-p (ntype)
  ;; In principle, the NULL ntype is also an EQL type.  But we do not treat
  ;; it as such, because the purpose of EQL ntypes is that they can be used
  ;; directly for constant folding.
  (not (%ntypep ntype)))

(defun ntype-union (ntype-1 ntype-2)
  (with-ntype-caching (ntype-1 ntype-2)
    (ntype
     `(or ,(type-specifier ntype-1)
          ,(type-specifier ntype-2)))))

(defun ntype-subtypep (ntype-1 ntype-2)
  (with-ntype-caching (ntype-1 ntype-2)
    (subtypep (type-specifier ntype-1)
              (type-specifier ntype-2))))

(defun ntype-subtypepc1 (ntype-1 ntype-2)
  (with-ntype-caching (ntype-1 ntype-2)
    (subtypep (type-specifier ntype-1)
              `(not ,(type-specifier ntype-2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Non-consing lists of Ntypes

(defun list-ntypes (&rest ntypes)
  "Behaves just like LIST, but avoids consing in some cases."
  ntypes)

(define-compiler-macro list-ntypes (&whole form &rest ntypes)
  (trivia:match ntypes
    ((list a) `(list-one-ntype ,a))
    ((list a b) `(lits-two-ntypes ,a ,b))
    (_ form)))

(defun list-one-ntype (ntype)
  (if (%ntypep ntype)
      (with-ntype-caching (ntype)
        (list ntype))
      (list ntype)))

(defun list-two-ntypes (ntype-1 ntype-2)
  (if (and (%ntypep ntype-1)
           (%ntypep ntype-2))
      (with-ntype-caching (ntype-1 ntype-2)
        (list ntype-1 ntype-2))
      (list ntype-1 ntype-2)))
