;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; This library provides two macros for aggressive memory reuse.
;;;
;;; (RECYCLING FORM)
;;;
;;; (RECYCLE TYPESPEC OBJECT)
;;;
;;; Usage example:
;;;
;;; (let ((tmp (recycling (make-array-list 10))))
;;;   (work-on tmp)
;;;   (recycle array-list tmp))

(defun recycling-expander-name (allocating-fn)
  (symbolicate '#:recycling-expander-for- allocating-fn))

(defun recycle-expander-name (typespec)
  (symbolicate '#:recycle-expander-for- typespec))

(defmacro define-recycling-expander (allocating-fn lambda-list &body body)
  "Define a recycling allocator for forms starting with ALLOCATING-FN."
  (check-type allocating-fn symbol)
  `(defmacro ,(recycling-expander-name allocating-fn) ,lambda-list ,@body))

(defmacro define-recycle-expander (typespec lambda-list &body body)
  "Define how to recycle objects of type TYPE-DESIGNATOR."
  (check-type typespec symbol)
  `(defmacro ,(recycle-expander-name typespec) ,lambda-list ,@body))

(defmacro recycling (&whole whole (access-fn &rest arguments))
  (check-type access-fn symbol)
  (let ((expander-name (recycling-expander-name access-fn)))
    (if (macro-function expander-name)
        `(,expander-name ,@arguments)
        whole)))

(defmacro recycle (&whole whole typespec object)
  (check-type typespec symbol)
  (let ((expander-name (recycle-expander-name typespec)))
    (if (macro-function expander-name)
        `(,expander-name ,object)
        whole)))

;;; recycling of conses

(defvar *cons-pool* (make-array 400 :fill-pointer 0))
(declaim (type (vector cons) *cons-pool*))

(define-recycling-expander cons (car cdr)
  (once-only (car cdr)
    (with-gensyms (pool cons)
      `(let ((,pool *cons-pool*))
         (if (zerop (fill-pointer ,pool))
             (cons ,car ,cdr)
             (let ((,cons (vector-pop ,pool)))
               (setf (car ,cons) ,car)
               (setf (cdr ,cons) ,cdr)
               ,cons))))))

(define-recycle-expander cons (object)
  (once-only (object)
    (with-gensyms (pool)
      `(let ((,pool *cons-pool*))
         (unless (= (fill-pointer ,pool)
                    (array-total-size ,pool))
           (vector-push ,object ,pool))
         (values)))))

;;; recycling of vectors

(deftype array-list () `(and (vector t) (not simple-vector)))

(defvar *array-list-pool* (make-array 100 :fill-pointer 0))
(declaim (type (vector array-list) *vector-pool*))

(declaim (inline make-array-list))
(defun make-array-list (length)
  (declare (type array-length length))
  (make-array length :fill-pointer t))

(define-recycling-expander make-array-list (length)
  (once-only (length)
    `(if (zerop (fill-pointer *vector-pool*))
         (make-vector ,length)
         (vector-pop *vector-pool*))))
