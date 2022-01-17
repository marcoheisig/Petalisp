;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Object Ntypes

(declaim (inline ntype-of))
(defun ntype-of (object)
  (if (%ntypep object)
      (ntype 't)
      object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ntype Generalization

(declaim (inline generalize-ntype))
(defun generalize-ntype (ntype)
  (if (%ntypep ntype)
      ntype
      (the (values ntype &optional)
           (%generalize-ntype ntype))))

(defun %generalize-ntype (ntype)
  (macrolet ((body ()
               `(typecase ntype
                  ,@(loop for ntype across *ntypes*
                          collect
                          `(,(%ntype-type-specifier ntype) ',ntype)))))
    (body)))


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
;;; Ntype Subtypecase

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
