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
     (defun ,(intern (concatenate 'string (symbol-name name) "?")) (x)
       (typep x ',name))))

(defun extended-euclid (u v)
  "Given nonnegative integers u and v, return the values u1 and u3 such
  that u*u1 + v*u2 = u3 = gcd(u,v)."
  (declare (type unsigned-byte u v))
  (labels
      ((bignum-euclid (u1 u3 v1 v3)
         (declare (type integer u1 v1)
                  (type unsigned-byte u3 v3))
         (if (zerop v3)
             (values u1 u3)
             (let ((q (floor u3 v3)))
               (bignum-euclid
                v1 v3
                (- u1 (* q v1))
                (- u3 (* q v3))))))
       (fixnum-euclid (u1 u3 v1 v3)
         (declare (type fixnum u1 v1)
                  (type (and unsigned-byte fixnum) u3 v3)
                  (optimize (speed 3) (safety 0)))
         (if (zerop v3)
             (values u1 u3)
             (let ((q (floor u3 v3)))
               (fixnum-euclid
                v1 v3
                (- u1 (the fixnum (* q v1)))
                (- u3 (the fixnum (* q v3))))))))
    (if (<= (* u v) ; crude estimate of (lcm u v)
            most-positive-fixnum)
        (fixnum-euclid 1 u 0 v)
        (bignum-euclid 1 u 0 v))))

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

;; make the symbol TODO a valid piece of lisp code
(defconstant TODO 'TODO)

(defun array-map (function array &rest more-arrays)
  (let ((n-elements (reduce #'* (array-dimensions array)))
        (result (copy-array array)))
    (loop for i below n-elements do
      (setf (row-major-aref result i)
            (apply function
                   (row-major-aref array i)
                   (mapcar #'(lambda (x) (row-major-aref x i)) more-arrays))))
    result))

(defun forever (value)
  "Make a one element circular list with a CAR of VALUE."
  (let ((x (list value)))
    (setf (cdr x) x)))
