;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun extended-euclid (u v)
  "Given nonnegative integers u and v, return the values u1 and u3 such
  that u*u1 + v*u2 = u3 = gcd(u,v)."
  (declare (non-negative-integer u v))
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

(defun function-lambda-list (function)
  "Return one of the following multiple values:
  (values LAMBDA-LIST T) where LAMBDA-LIST is the lambda list of FUNCTION
  (values NIL NIL) if the lambda list could not be determined."
  #+allegro (values (excl:arglist function) t)
  #+clisp (values (sys::arglist function) t)
  #+(or cmu scl)
  (let ((f (coerce function 'function)))
    (typecase f
      (standard-generic-function (values (pcl:generic-function-lambda-list f) t))
      (eval:interpreted-function (values (eval:interpreted-function-arglist f) t))
      (function (values (read-from-string (kernel:%function-arglist f)) t))
      (t (values nil nil))))
  #+cormanlisp (values (ccl:function-lambda-list
                        (typecase function
                          (symbol (fdefinition function))
                          (t function))) t)
  #+gcl (let ((f (etypecase function
                   (symbol function)
                   (function (si:compiled-function-name function)))))
          (values (get f 'si:debug) t))
  #+lispworks (values (lw:function-lambda-list function) t)
  #+lucid (values (lcl:arglist function) t)
  #+sbcl (values (sb-introspect:function-lambda-list function) t)
  #-(or allegro clisp cmu scl cormanlisp gcl lispworks lucid sbcl)
  (values nil nil))

(defun check-arity (function arity)
  "Signal a simple-program-error if FUNCTION cannot be called with ARITY arguments."
  (multiple-value-bind (lambda-list lambda-list?) (function-lambda-list function)
      (when lambda-list?
        (let ((mandatory-arguments 0)
              (max-arguments 0)
              (upper-bound? t)
              (mandatory-increment 1)
              (max-increment 1))
          (declare (type (integer 0 #.call-arguments-limit) arity
                         mandatory-arguments max-arguments
                         mandatory-increment max-increment)
                   (boolean upper-bound?))
          (dolist (item lambda-list)
            (case item
              (&key      (setf max-increment 2) (setf mandatory-increment 0))
              (&optional (setf max-increment 1) (setf mandatory-increment 0))
              (&aux      (setf max-increment 0) (setf mandatory-increment 0))
              ((&rest &allow-other-keys)
               (setf max-increment 0)
               (setf mandatory-increment 0)
               (setf upper-bound? nil))
              (t
               (incf mandatory-arguments mandatory-increment)
               (incf max-arguments max-increment))))
          (when (< arity mandatory-arguments)
            (simple-program-error
             "Only ~R argument~:P given for a function with ~R mandatory argument~:P."
             arity mandatory-arguments))
          (when (and upper-bound? (> arity max-arguments))
            (simple-program-error
             "Received ~R argument~:P for a function that accepts at most ~R argument~:P."
             arity max-arguments))))))

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

(defun successor-table (graph-roots predecessors)
  "Given a sequence of GRAPH-ROOTS and a function to determine the sequence
  of predecessors of each graph node, return a hash-table mapping each node
  to a list of its successors."
  (let ((table (make-hash-table :test #'eq)))
    (flet ((push-entry (key hash-table value)
             (if (consp (gethash key hash-table))
                 (push value (gethash key hash-table))
                 (setf (gethash key hash-table) (list value)))))
      (map nil (named-lambda populate-successor-table (node)
                 (let ((predecessors (funcall predecessors node)))
                   ;; check whether node is visited for the first time
                   (unless (member node (gethash (elt predecessors 0) table))
                     (map nil #'(lambda (predecessor)
                                  (push-entry predecessor table node))
                          predecessors))))
           graph-roots)
      table)))
