;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Array Ntypes

(defun array-element-ntype (array)
  (macrolet ((body ()
               (let ((ntypes (remove-duplicates
                              *ntypes*
                              :test #'alexandria:type=
                              :key (alexandria:compose
                                    #'upgraded-array-element-type
                                    #'%ntype-type-specifier))))
                 `(etypecase array
                    ,@(loop for ntype across ntypes
                            collect
                            `((array ,(%ntype-type-specifier ntype)) ',ntype))))))
    (body)))

(defun make-rank-zero-array (value)
  (macrolet ((body ()
               (let ((ntypes (remove-duplicates
                              *ntypes*
                              :test #'alexandria:type=
                              :key (alexandria:compose
                                    #'upgraded-array-element-type
                                    #'%ntype-type-specifier))))
                 `(etypecase value
                    ,@(loop for ntype across ntypes
                            collect
                            `(,(%ntype-type-specifier ntype)
                              (make-array '() :element-type ',(%ntype-type-specifier ntype)
                                              :initial-element value)))))))
    (body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reasoning About Ntypes

(declaim (inline %ntype=))
(defun %ntype= (ntype-1 ntype-2)
  (declare (ntype ntype-1 ntype-2))
  (= (%ntype-id ntype-1)
     (%ntype-id ntype-2)))

(declaim (inline ntype=))
(defun ntype= (ntype-1 ntype-2)
  ;; Checking for ntype equality is slightly complicated by the fact that
  ;; NULL is a singleton type.
  (let ((null-ntype (load-time-value (generalize-ntype (ntype 'null)))))
    (cond ((null ntype-1)
           (if (%ntypep ntype-2)
               (%ntype= ntype-2 null-ntype)
               (null ntype-2)))
          ((null ntype-2)
           (if (%ntypep ntype-1)
               (%ntype= ntype-1 null-ntype)
               (null ntype-1)))
          ((%ntypep ntype-1)
           (if (%ntypep ntype-2)
               (%ntype= ntype-1 ntype-2)
               nil))
          ((%ntypep ntype-2)
           nil)
          (t (eql ntype-1 ntype-2)))))

(defun empty-ntype-p (ntype)
  (and (%ntypep ntype)
       (ntype= ntype (ntype 'nil))))

(defun universal-ntype-p (ntype)
  (and (%ntypep ntype)
       (ntype= ntype (ntype 't))))

(defun eql-ntype-p (ntype)
  ;; In principle, the NULL ntype is also an EQL type.  But we do not treat
  ;; it as such, because the purpose of EQL ntypes is that they can be used
  ;; directly as their value.
  (not (%ntypep ntype)))

(defun ntype-union (&rest ntypes)
  (flet ((two-argument-ntype-union (ntype-1 ntype-2)
           (if (and (eql-ntype-p ntype-1)
                    (eql-ntype-p ntype-2)
                    (eql ntype-1 ntype-2))
               ntype-1
               (with-ntype-caching (ntype-1 ntype-2)
                 (ntype
                  `(or ,(type-specifier ntype-1)
                       ,(type-specifier ntype-2)))))))
    (if (null ntypes)
        (ntype 'nil)
        (reduce #'two-argument-ntype-union ntypes))))

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
