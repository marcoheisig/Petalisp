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

(defconstant type-code-id-limit
  (length (all-type-code-type-specifiers)))

(deftype type-code-id ()
  `(integer 0 (,type-code-id-limit)))

(deftype type-cache (n)
  `(simple-array t ,(loop repeat n collect type-code-id-limit)))

(defconstant +type-code-ids+
  (if (boundp '+type-code-ids+)
      +type-code-ids+
      (let ((type-specifiers (all-type-code-type-specifiers)))
        (map '(simple-array (unsigned-byte 8) (*))
             (lambda (type-code)
               (position (uncached-type-specifier-from-type-code type-code)
                         type-specifiers
                         :test #'subtypep))
             (all-type-codes)))))

(declaim (inline type-code-id))
(defun type-code-id (type-code)
  (declare (type-code type-code))
  (the type-code-id (aref +type-code-ids+ type-code)))

(defun type-code-from-type-code-id (type-code-id)
  (declare (type-code-id type-code-id))
  (position type-code-id +type-code-ids+))

(defmacro define-type-code-cache (name lambda-list &body body)
  (let ((n (length lambda-list)))
    `(progn
       (declaim (type (type-cache ,n) ,name))
       (defparameter ,name
         (flet ((fn ,lambda-list ,@body))
           (let ((cache (make-array ',(loop repeat n collect type-code-id-limit))))
             (labels ((rec (n ids)
                        (if (= n 0)
                            (setf (apply #'aref cache ids)
                                  (apply #'fn (mapcar #'type-code-from-type-code-id ids)))
                            (loop for id below type-code-id-limit do
                              (rec (1- n) (cons id ids))))))
               (rec ,n '()))
             cache))))))

(defmacro access-type-code-cache (cache &rest type-codes)
  (assert (every #'symbolp type-codes))
  (let ((n (length type-codes)))
    (alexandria:once-only (cache)
      `(locally (declare (type-code ,@type-codes)
                         (type (type-cache ,n) ,cache)
                         (optimize (speed 3) (safety 0)))
         (aref ,cache
               ,@(mapcar
                  (lambda (type-code) `(type-code-id ,type-code))
                  type-codes))))))

