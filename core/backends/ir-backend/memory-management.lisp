;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-ir-backend)

;;; A hash table, mapping from symbols to memory locations.  Each memory
;;; location is represented as a cons cell, whose car holds the value and
;;; whose cdr is the symbol used to look up the memory location.  The cdr
;;; is for debugging purposes only.
(defvar *memory-location-table*)

;;; Return a fresh memory location for SYMBOL.  It is an error to call this
;;; function twice for the same symbol.
(defun fresh-memory-location (symbol)
  (check-type symbol symbol)
  (assert (not (nth-value 1 (gethash symbol *memory-location-table*))))
  (setf (gethash symbol *memory-location-table*)
        (cons nil symbol)))

;;; Return the memory location corresponding to SYMBOL.  It is an error to
;;; call this function for a symbol that has not yet been introduced via
;;; FRESH-MEMORY-LOCATION.
(defun memory-location (symbol)
  (check-type symbol symbol)
  (multiple-value-bind (memory-location present-p)
      (gethash symbol *memory-location-table*)
    (assert present-p)
    memory-location))

;;; Return a function that takes an index and returns the value
;;; of the given LOAD for this particular index.
(defun make-load-thunk (load)
  (if (symbolp load)
      (let ((cons (memory-location load)))
        (lambda (index)
          (declare (ignore index))
          (car cons)))
      (destructuring-bind (buffer . transformation) load
        (let ((storage (storage buffer)))
          (lambda (index)
            (apply #'aref storage (transform index transformation)))))))

;;; Return a function that takes an index and a value and that stores the
;;; given value at the location indicated by STORE.
(defun make-store-thunk (store)
  (if (symbolp store)
      (if (null store)
          (lambda (index value)
            (declare (ignore index value))
            (values))
          (let ((cons (fresh-memory-location store)))
            (lambda (index value)
              (declare (ignore index))
              (setf (car cons) value))))
      (destructuring-bind (buffer . transformation) store
        (let ((storage (storage buffer)))
          (lambda (index value)
            (setf (apply #'aref storage (transform index transformation))
                  value))))))
