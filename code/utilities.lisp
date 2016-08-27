;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmacro define-class (name direct-superclasses slots &rest options)
  "Defines a class using DEFCLASS, where all slot-specifiers that consist
only of a single symbol are expanded to define a :initarg keyword and a
reader of the same name. Additionally, defines a <NAME>-P predicate."
  `(progn
     (defclass ,name ,direct-superclasses
       ,(loop for slot in slots
              collect
              (if (symbolp slot)
                  `(,slot :initarg ,(make-keyword slot) :reader ,slot)
                  slot))
       ,@options)
     (defun ,(intern (concatenate 'string (symbol-name name) "-P")) (x)
       (typep x ',name))))

(defun kuṭṭaka (d1 d2 c)
  "Returns A, B and GCD(d1,d2) such that A * d1 - B * d2 = c. Returns false
if no solution exists."
  (declare (integer d1 d2 c))
  ;; The first part is just Euclids algorithm to determine the GCD, where
  ;; we additionally keep track of all the quotients
  (let* ((quotients ())
         (gcd
           (loop with u of-type integer = (abs d1)
                 and  v of-type integer = (abs d2) do
                   (when (= v 0) (return u))
                   (multiple-value-bind (quot rem) (floor u v)
                     (push quot quotients)
                     (psetf v rem u v)))))
    ;; If C cannot be divided by GCD, there is no solution
    (let ((c (/ c gcd)))
      (unless (integerp c) (return-from kuṭṭaka nil))
      ;; now comes the algorithm of Aryabhata
      (let* ((a 0)
             (b (if (evenp (length quotients)) c (- c))))
        (mapc
         (lambda (x)
           (psetf a b b (the integer (+ (* x b) a))))
         (cdr quotients))
        (values a b gcd)))))

(defun identical (list &key (test #'eql) (key #'identity))
  (or (null list)
      (let ((reference-element (funcall key (car list))))
        (every
         (lambda (item)
           (funcall test reference-element (funcall key item)))
         (cdr list)))))

(defmacro zapf (place expr)
  (multiple-value-bind
        (temps exprs stores store-expr access-expr)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores)
              (let ((% ,access-expr))
                ,expr)))
       ,store-expr)))

(defun mapt (function tree)
  (cond
    ((atom tree)
     (funcall function tree)
     nil)
    ((consp tree)
     (mapt function (car tree))
     (and (consp (cdr tree))
          (mapt function (cdr tree))))))

(defun tree-find-if (function tree)
  (mapt (lambda (x)
          (when (funcall function x)
            (return-from tree-find-if x)))
        tree))

(defconstant TODO 'TODO) ; make TODO a valid piece of lisp code
