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
;;; Caching of Ntype Operations

(let ((id-limit (1+ (loop for ntype across *ntypes*
                          maximize (%ntype-id ntype)))))

  (deftype ntype-cache (rank)
    `(simple-array t ,(loop repeat rank collect id-limit)))

  (defun make-ntype-cache (rank fn)
    (let ((cache (make-array (loop repeat rank collect id-limit))))
      (labels ((rec (n ids)
                 (if (zerop n)
                     (setf (apply #'aref cache ids)
                           (apply fn (mapcar (lambda (id) (aref *ntypes* id)) ids)))
                     (loop for id below id-limit do
                       (rec (1- n) (cons id ids))))))
        (rec rank '()))
      cache)))

(defmacro with-ntype-caching (ntypes &body body)
  (assert (null (intersection ntypes lambda-list-keywords)))
  (assert (every #'symbolp ntypes))
  (let* ((n (length ntypes))
         (cache (gensym "CACHE"))
         (indices (loop repeat n collect (gensym "INDEX"))))
    `(let ((,cache
             (load-time-value
              (make-ntype-cache ,n (lambda ,ntypes ,@body))))
           ,@(loop for index in indices
                   for ntype in ntypes
                   collect
                   `(,index (%ntype-id (generalize-ntype ,ntype)))))
       (declare (optimize (speed 3) (safety 0))
                (type (ntype-cache ,n) ,cache))
       (aref ,cache ,@indices))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reasoning About Ntypes

(declaim (inline ntype-of))
(defun ntype-of (object)
  (if (%ntypep object)
      (ntype 't)
      object))

(declaim (inline generalize-ntype))
(defun generalize-ntype (object)
  (if (%ntypep object)
      object
      (macrolet ((body ()
                   `(typecase object
                      ,@(loop for ntype across *ntypes*
                              collect
                              `(,(%ntype-type-specifier ntype) ',ntype)))))
        (body))))

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

(defmacro ntype-subtypecase (ntype &body clauses &environment env)
  (let ((checked-bits 0)
        (id (gensym "ID"))
        (cond-clauses '()))
    (loop for (type-specifier . body) in clauses do
      (let ((mask (logandc2
                   (ntype-subtypep-mask type-specifier env)
                   checked-bits)))
        (unless (zerop mask)
          (push `((logbitp ,id ,mask) ,@body) cond-clauses)
          (setf checked-bits (logior checked-bits mask)))))
    (unless (= checked-bits (ntype-subtypep-mask t))
      (warn "The provided NTYPE-SUBTYPECASE clauses do not cover all types.~@
             The following types are missing:~%~{ ~S~%~}"
            (loop for ntype across *ntypes*
                  unless (logbitp (%ntype-id ntype) checked-bits)
                    collect (%ntype-type-specifier ntype))))
    (alexandria:once-only (ntype)
      `(let ((,id (%ntype-id (generalize-ntype ,ntype))))
         (cond ,@(reverse cond-clauses))))))

(defun ntype-subtypep-mask (type-specifier &optional env)
  (loop for ntype across *ntypes*
        sum (if (subtypep (%ntype-type-specifier ntype) type-specifier env)
                (ash 1 (%ntype-id ntype))
                0)))
