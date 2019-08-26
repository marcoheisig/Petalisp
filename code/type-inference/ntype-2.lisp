;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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
      (macrolet ((body ()
                   `(typecase ntype
                      ,@(loop for ntype across *ntypes*
                              collect
                              `(,(%ntype-type-specifier ntype) ',ntype)))))
        (body))))

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
