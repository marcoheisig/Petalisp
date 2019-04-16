;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

;;; With just 128 elements, the space of type codes is very small.  This
;;; makes it possible to resolve most type code related queries with a
;;; lookup table.  The size of such a table can be further shrunk by
;;; realizing that many different type codes correspond to the same actual
;;; type of the Lisp implementation.  So each cache lookup consists of two
;;; parts - going from the given type code to its id - the unique number of
;;; the corresponding type - and then using the id as the key of the lookup
;;; table.

(alexandria:define-constant +type-code-ids+
    (let ((type-specifiers (all-type-code-type-specifiers)))
      (map '(simple-array (unsigned-byte 8) (*))
           (lambda (type-code)
             (position (uncached-type-specifier-from-type-code type-code)
                       type-specifiers
                       :test #'equal
                       :from-end t))
           (all-type-codes)))
  :test #'equalp)

(alexandria:define-constant +type-code-id-limit+
    (1+ (loop for id across +type-code-ids+ maximize id)))

(alexandria:define-constant +id-type-codes+
    (map '(simple-array (unsigned-byte 8) (*))
         (lambda (id)
           (position id +type-code-ids+ :from-end t))
         (alexandria:iota +type-code-id-limit+))
  :test #'equalp)

(deftype type-code-id ()
  `(integer 0 (,+type-code-id-limit+)))

(deftype type-cache (n)
  `(simple-array t ,(loop repeat n collect +type-code-id-limit+)))

(declaim (inline type-code-id-from-type-code))
(defun type-code-id-from-type-code (type-code)
  (declare (type-code type-code))
  (the type-code-id (aref +type-code-ids+ type-code)))

(defun type-code-from-type-code-id (type-code-id)
  (declare (type-code-id type-code-id))
  (the type-code (aref +id-type-codes+ type-code-id)))

(defun make-type-code-cache (dimension fn)
  (let ((cache (make-array (loop repeat dimension collect +type-code-id-limit+))))
    (labels ((rec (n ids)
               (if (= n 0)
                   (setf (apply #'aref cache ids)
                         (apply fn (mapcar #'type-code-from-type-code-id ids)))
                   (loop for id below +type-code-id-limit+ do
                     (rec (1- n) (cons id ids))))))
      (rec dimension '()))
    cache))

(defmacro with-type-code-caching (type-codes &body body)
  (assert (every #'symbolp type-codes))
  (assert (null (intersection type-codes lambda-list-keywords)))
  (let ((n (length type-codes))
        (cache (gensym "CACHE")))
    `(let ((,cache (load-time-value
                    (make-type-code-cache
                     ,n
                     (lambda ,type-codes ,@body)))))
       (declare (type-code ,@type-codes)
                (type (type-cache ,n) ,cache)
                (optimize (speed 3) (safety 0)))
       (aref ,cache
               ,@(mapcar
                  (lambda (type-code) `(type-code-id-from-type-code ,type-code))
                  type-codes)))))
