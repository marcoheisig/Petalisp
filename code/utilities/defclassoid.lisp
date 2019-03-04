;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(declaim (class *classoid-metaclass*))
(defvar *classoid-metaclass* (find-class 'structure-class))

(deftype non-nil-symbol ()
  '(and symbol (not null)))

(deftype function-name ()
  '(or non-nil-symbol
    (cons (eql setf)
     (cons symbol null))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary Classes
;;;
;;; The following classes are used to compute the expansion of DEFCLASSOID.
;;; It's instances are immediately discarded after macro expansion.

(defun expand-classoid (classoid)
  (expand-classoid-using-metaclass classoid *classoid-metaclass*))

(defgeneric expand-classoid-using-metaclass (classoid metaclass))

(defclass classoid ()
  ((%name :initarg :name :reader classoid-name :type symbol)
   ;; The name of the superclass of the classoid, or NIL.
   (%superclass :initform nil :accessor classoid-superclass)
   ;; A string, or NIL.
   (%documentation :initform nil :accessor classoid-documentation)
   ;; A list of classoid-constructors.
   (%constructors :initform '() :accessor classoid-constructors)
   ;; A list of classoid-slots.
   (%slots :initform '() :accessor classoid-slots)
   ;; A list of symbols, one for each desired predicate.
   (%predicates :initform '() :accessor classoid-predicates)
   ;; A list of symbols, one for each desired copier.
   (%copiers :initform '() :accessor classoid-copiers)))

(defclass classoid-slot ()
  ((%name :initarg :name :reader classoid-slot-name :type symbol)
   (%initargs :accessor classoid-slot-initargs :initform '())
   (%readers :accessor classoid-slot-readers :initform '())
   (%writers :accessor classoid-slot-writers :initform '())
   (%initform :accessor classoid-slot-initform)
   (%type :accessor classoid-slot-type)
   (%documentation :accessor classoid-slot-documentation)))

(defclass classoid-constructor ()
  ((%name :initarg :name :reader classoid-constructor-name :type symbol)
   (%lambda-list :initarg :lambda-list :reader classoid-constructor-lambda-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The DEFCLASSOID Macro

(defmacro defclassoid (name direct-superclasses direct-slots &rest options)
  (expand-classoid
   (parse-classoid name direct-superclasses direct-slots options)))

(defun parse-classoid (name direct-superclasses direct-slots options)
  (check-type name non-nil-symbol)
  (let ((classoid (make-instance 'classoid :name name)))
    (with-accessors ((superclass classoid-superclass)
                     (documentation classoid-documentation)
                     (constructors classoid-constructors)
                     (slots classoid-slots)
                     (predicates classoid-predicates)
                     (copiers classoid-copiers))
        classoid
      ;; Parse list of superclasses.
      (trivia:match direct-superclasses
        ((list))
        ((list (and superclass-name (type (and symbol (not null)))))
         (setf superclass superclass-name))
        (_
         (error "~@<Invalid list of superclasses:~%~S~%~
                    The list of superclasses given to DEFCLASSOID must be either ~
                    empty, or a list containing the name of another classoid.~:@>"
                direct-superclasses)))
      ;; Parse slots.
      (setf slots (mapcar #'parse-classoid-slots direct-slots))
      ;; Parse options.

      ;; Done.
      classoid)))

(defun parse-classoid-slot (slot)
  (unless (and (consp slot)
               (symbolp (first slot))
               (evenp (length (rest slot))))
    (error "Invalid slot specification:~%~S" slot))
  (let ((classoid-slot (make-instance 'classoid-slot :name (first slot))))
    (with-accessors ((initargs classoid-slot-initargs)
                     (initform classoid-slot-initform)
                     (readers classoid-slot-readers)
                     (writers classoid-slot-writers)
                     (type classoid-slot-type)
                     (documentation classoid-slot-documentation))
        classoid-slot
      (loop for (key value) on (rest slot) by #'cddr do
        (case key
          ((nil) (loop-finish))
          (:reader
           (check-type value non-nil-symbol)
           (push value readers))
          (:writer
           (check-type value function-name)
           (push value writers))
          (:accessor
           (check-type value non-nil-symbol)
           (push value readers)
           (push `(setf ,value) writers))
          (:initform
           (when (slot-boundp classoid-slot '%initform)
             (error "Duplicate slot initforms:~%~S" slot))
           (setf initform value))
          (:initarg
           (check-type value symbol)
           (push value initargs))
          (:type
           (when (slot-boundp classoid-slot '%type)
             (error "Duplicate slot types:~%~S" slot))
           (setf type value))
          (:documentation
           (when (slot-boundp classoid-slot '%documentation)
             (error "Duplicate slot documentation:~%~S" slot))
           (check-type value string)
           (setf documentation value))
          (otherwise
           (error "Invalid slot option:~%~S" key))))
      classoid-slot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary Methods
