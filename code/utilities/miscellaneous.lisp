;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun extended-euclid (u v)
  "Given nonnegative integers u and v, return the values u1 and u3 such
  that u*u1 + v*u2 = u3 = gcd(u,v)."
  (declare (unsigned-byte u v))
  (labels
      ((bignum-euclid (u1 u3 v1 v3)
         (declare (integer u1 u3 v1 v3))
         (if (zerop v3)
             (values u1 u3)
             (let ((q (floor u3 v3)))
               (bignum-euclid
                v1 v3
                (- u1 (* q v1))
                (- u3 (* q v3))))))
       (fixnum-euclid (u1 u3 v1 v3)
         (declare (fixnum u1 u3 v1 v3)
                  (optimize (speed 3) (safety 0)))
         (if (zerop v3)
             (values u1 u3)
             (let ((q (floor u3 v3)))
               (fixnum-euclid
                v1 v3
                (- u1 (the fixnum (* q v1)))
                (- u3 (the fixnum (* q v3))))))))
    (if (<= (* u v) most-positive-fixnum) ; cheap estimate of (lcm u v)
        (fixnum-euclid 1 u 0 v)
        (bignum-euclid 1 u 0 v))))

(test |(extended-euclid)|
  (flet ((? (u v)
           (multiple-value-bind (u1 u3) (extended-euclid u v)
             (is (= (gcd u v) u3))
             (if (zerop v)
                 (is (= u3 (* u u1)))
                 (is (integerp (/ (- u3 (* u u1)) v)))))))
    (? 0 0)
    (? 1 0)
    (? 0 1)
    (? (expt 6 40) (expt 9 40))
    (for-all ((u (integer-generator 0))
              (v (integer-generator 0)))
      (? u v))))

(defun identical (sequence &key (test #'eql) (key #'identity))
  "Check whether the KEYs of SEQUENCE are identical with respect to TEST."
  (etypecase sequence
    (list
     (or (null sequence)
         (loop :with reference-element := (funcall key (car sequence))
               :for element :in (cdr sequence)
               :always (funcall test
                                reference-element
                                (funcall key element)))))
    (simple-vector #1=
     (or (= 0 (length sequence))
         (loop :with reference-element := (funcall key (elt sequence 0))
               :for i :from 1 :below (length sequence)
               :always (funcall test
                                reference-element
                                (funcall key (elt sequence i))))))
    (sequence #1#)))

(test |(identical)|
  (is-true (identical '(1 1 1)))
  (is-true (identical #(1 1.0 1/1) :test #'=))
  (is-true (identical '(1 2.0 6/2 4d0) :key #'numberp :test #'eq))
  (is-true (identical "aaaAaa" :test #'char= :key #'char-upcase)))

(defun array-map (function result &rest arrays)
  (check-type function function)
  (assert (identical (cons result arrays) :key #'array-dimensions :test #'equal)
          (arrays)
          "Arguments have different ARRAY-DIMENSIONS:~%~{  ~S~%~}" arrays)
  (dotimes (i (array-total-size result))
    (setf (row-major-aref result i)
          (apply function (mapcar
                           #'(lambda (x) (row-major-aref x i))
                           arrays))))
  result)

(test |(array-map)|
  (is (equalp
       #2a((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))
       (array-map
        #'+
        (make-array '(4 4))
        #2a((1 1 1 1) (1 1 1 1) (1 1 1 1) (1 1 1 1))
        #2a((0 1 2 3) (0 1 2 3) (0 1 2 3) (0 1 2 3))))))

(defun list-of-symbols (length)
  (loop :for i :below length
        :with abc := #(a b c d e f g h i j k l m n o p q r s t. u v w x y z)
        :collect (if (< i (length abc))
                     (aref abc i)
                     (format-symbol t "VAR~D" i))))

(defun check-arity (function arity)
  "Signal an error if FUNCTION cannot be called with ARITY arguments."
  (declare (ignorable function arity))
  )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  JIT compilation of compute intensive kernels

(defvar *kernels* (make-hash-table :test #'equalp))

(defvar *kernel-definitions* (make-hash-table :test #'eq))

(defun redefine-kernel (name macroexpander)
  (let ((previous-macroexpander (gethash name *kernel-definitions*)))
    (unless (eq macroexpander previous-macroexpander)
      ;; install new definition
      (setf (gethash name *kernel-definitions*) macroexpander)
      ;; invalidate old kernels
      (mapc
       (lambda (key)
         (remhash key *kernels*))
       (loop for key being the hash-keys of *kernels*
             when (eq (first key) name) collect key)))))

(defun get-kernel (name &rest args)
  (or (gethash (cons name args) *kernels*)
      (progn
        #+nil(print (apply (gethash name *kernel-definitions*) args))
        (setf (gethash (cons name args) *kernels*)
              (compile
               nil
               (apply (gethash name *kernel-definitions*) args))))))

(defmacro defkernel (name lambda-list &body body)
  (let ((variables
          (remove-if
           (lambda (x) (find x lambda-list-keywords))
           (flatten lambda-list))))
    `(progn
       (redefine-kernel
        ',name
        (lambda ,variables ,@body))
       (defun ,name ,lambda-list
         (get-kernel ',name ,@variables)))))
