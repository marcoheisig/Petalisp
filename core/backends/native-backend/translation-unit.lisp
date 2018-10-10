;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

(defclass translation-unit ()
  (;; A list of types, one for each storage in use.
   (%storage-types :initarg :storage-types :reader storage-types)
   ;; The outermost basic block of this translation unit.
   (%initial-basic-block :initarg :initial-basic-block :reader initial-basic-block)
   ;; A hash table, mapping from variables to their defining basic blocks.
   (%basic-block-index-table :reader basic-block-index-table
                             :initform (make-hash-table :test #'eq))
   ;; A hash table.  Whenever a variable has been removed from the program
   ;; because it carries the same value as another variable, this table has
   ;; an entry mapping from the removed variable to the other variable.
   (%alias-table :reader alias-table
                 :initform (make-hash-table :test #'eq))))

;;; Initialize the basic block table of a translation unit.
(defmethod shared-initialize :after ((translation-unit translation-unit) slot-names &rest args)
  (declare (ignore slot-names args))
  (with-accessors ((basic-blocks basic-blocks)
                   (basic-block-index-table basic-block-index-table))
      translation-unit
    (labels ((register-basic-block (basic-block)
               (setf (gethash (index-variable (depth basic-block))
                              basic-block-index-table)
                     basic-block)
               (when (successor basic-block)
                 (register-basic-block
                  (successor basic-block)))))
      (register-basic-block
       (successor
        (initial-basic-block translation-unit))))))

(defclass basic-block ()
  ((%instructions :initform '() :accessor instructions)
   (%successor :initarg :successor :reader successor)))

(defclass range-block (basic-block)
  ((%depth :initarg :depth :reader depth)
   (%fixnum-p :initarg :fixnum-p :reader fixnum-p)))

(defclass loop-block (range-block)
  ())

(defclass reduction-block (range-block)
  ((%operator :initform :operator :reader operator)
   (%stores :initform :stores :reader stores)))

(defun make-basic-block (&optional successor)
  (make-instance 'basic-block
    :successor successor))

(defun make-loop-block (fixnum-p lb ub depth &optional successor)
  (declare (ignore lb ub))
  (make-instance 'loop-block
    :depth depth
    :fixnum-p fixnum-p
    :successor successor))

(defun make-reduction-block (stores operator fixnum-p lb ub depth &optional successor)
  (declare (ignore lb ub))
  (make-instance 'reduction-block
    :operator operator
    :stores stores
    :depth depth
    :fixnum-p fixnum-p
    :successor successor))
