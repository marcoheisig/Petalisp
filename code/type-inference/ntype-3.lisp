;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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
                            `((array ,(%ntype-type-specifier ntype)) ',ntype))
                    (array (ntype (array-element-type array)))))))
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

(declaim (inline ntype-bits))
(defun ntype-bits (ntype)
  (%ntype-bits
   (generalize-ntype ntype)))

(declaim (inline ntype-id))
(defun ntype-id (ntype)
  (%ntype-id
   (generalize-ntype ntype)))

(declaim (inline %ntype=))
(defun %ntype= (ntype1 ntype2)
  (declare (ntype ntype1 ntype2))
  (= (%ntype-id ntype1)
     (%ntype-id ntype2)))

(declaim (inline ntype=))
(defun ntype= (ntype1 ntype2)
  ;; Checking for ntype equality is slightly complicated by the fact that
  ;; NULL is a singleton type.
  (let ((null-ntype (load-time-value (generalize-ntype (ntype 'null)))))
    (cond ((null ntype1)
           (if (%ntypep ntype2)
               (%ntype= ntype2 null-ntype)
               (null ntype2)))
          ((null ntype2)
           (if (%ntypep ntype1)
               (%ntype= ntype1 null-ntype)
               (null ntype1)))
          ((%ntypep ntype1)
           (if (%ntypep ntype2)
               (%ntype= ntype1 ntype2)
               nil))
          ((%ntypep ntype2)
           nil)
          (t (eql ntype1 ntype2)))))

(defun ntype< (ntype1 ntype2)
  (< (ntype-id ntype1)
     (ntype-id ntype2)))

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
  (flet ((two-argument-ntype-union (ntype1 ntype2)
           (if (and (eql-ntype-p ntype1)
                    (eql-ntype-p ntype2)
                    (eql ntype1 ntype2))
               ntype1
               (with-ntype-caching (ntype1 ntype2)
                 (ntype
                  `(or ,(type-specifier ntype1)
                       ,(type-specifier ntype2)))))))
    (if (null ntypes)
        (ntype 'nil)
        (reduce #'two-argument-ntype-union ntypes))))

(defun ntype-subtypep (ntype1 ntype2)
  (if (eql-ntype-p ntype1)
      (if (eql-ntype-p ntype2)
          (eql ntype1 ntype2)
          (typep ntype1 (type-specifier ntype2)))
      (if (eql-ntype-p ntype2)
          (ntype= ntype1 ntype2)
          (with-ntype-caching (ntype1 ntype2)
            (subtypep (type-specifier ntype1)
                      (type-specifier ntype2))))))

(defun ntype-subtypepc2 (ntype1 ntype2)
  (if (eql-ntype-p ntype1)
      (if (eql-ntype-p ntype2)
          (not (eql ntype1 ntype2))
          (typep ntype1 `(not ,(type-specifier ntype2))))
      (with-ntype-caching (ntype1 ntype2)
        (subtypep (type-specifier ntype1)
                  `(not ,(type-specifier ntype2))))))

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

(defun list-two-ntypes (ntype1 ntype2)
  (if (and (%ntypep ntype1)
           (%ntypep ntype2))
      (with-ntype-caching (ntype1 ntype2)
        (list ntype1 ntype2))
      (list ntype1 ntype2)))
