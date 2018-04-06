;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/utilities/miscellaneous
  (:use :closer-common-lisp :alexandria :agnostic-lizard :trivia)
  (:export
   #:asterisks
   #:identical
   #:free-variables
   #:type-specifier
   #:symbolic-+
   #:symbolic-*))

(in-package :petalisp/utilities/miscellaneous)

(defun asterisks (n)
  (make-list n :initial-element '*))

(deftype type-specifier () '(or symbol cons))

(defun identical (sequence &key (test #'eql) (key #'identity))
  "Check whether the KEYs of SEQUENCE are identical with respect to TEST."
  (etypecase sequence
    (list
     (or (null sequence)
         (loop with reference-element = (funcall key (car sequence))
               for element in (cdr sequence)
               always (funcall test
                               reference-element
                               (funcall key element)))))
    (simple-vector #1=
     (or (= 0 (length sequence))
         (loop with reference-element = (funcall key (elt sequence 0))
               for i from 1 below (length sequence)
               always (funcall test
                               reference-element
                               (funcall key (elt sequence i))))))
    (sequence #1#)))

(defun free-variables (form &optional environment)
  (let (result)
    (walk-form
     form environment
     :on-every-atom
     (lambda (form env)
       (prog1 form
         (when (and (symbolp form)
                    (not (find form (metaenv-variable-like-entries env) :key #'first)))
           (pushnew form result)))))
    result))

(defun symbolic-+ (&rest forms)
  (match (remove 0 forms)
    ((list) 0)
    ((list form) form)
    ( list `(+ ,@list))))

(defun symbolic-* (&rest forms)
  (or (find 0 forms)
      (match (remove 1 forms)
        ((list) 1)
        ((list form) form)
        ( list `(* ,@list)))))
