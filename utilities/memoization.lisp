;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/utilities/memoization
  (:use :closer-common-lisp :alexandria :trivial-garbage)
  (:export
   #:with-memoization
   #:with-multiple-value-memoization
   #:with-hash-table-memoization
   #:with-vector-memoization
   #:with-multiple-value-vector-memoization
   #:define-symbol-pool))

(in-package :petalisp/utilities/memoization)

;;; Marco Heisig's Memoization Macros
;;;
;;; There are numerous libraries for memoization, but most of them succumb
;;; the temptation to provide a wrapper for DEFUN as primary API. This
;;; fails to address several important use-cases
;;;
;;; - Memoization within CLOS, e.g. in :around methods
;;; - Explicit construction of the memoization key
;;; - Explicit specification of the memoization test
;;;
;;; To address these problems, this library uses lexical memoization, i.e.
;;; WITH-something macros that memoize the result of evaluating their
;;; body. Furthermore, there are different memoization macros for more or
;;; less fine-grained control over the memoization strategy.
;;;
;;; Thread safety is deliberately neglected. Unconditional thread safety is
;;; relatively expensive (but please, run your own benchmarks) and other
;;; solutions are necessarily application specific. One could even state
;;; that thread-safety is an ill-defined term, because in an application
;;; where only one thread calls a particular memoizing function, this
;;; library IS thread safe.

(defmacro with-memoization
    ((key &key (test nil test-p) (store-key nil store-key-p)) &body body)
  "Memoize the value of BODY. If KEY has the same value (with respect to
  TEST) as some previously computed key, then BODY is not evaluated and the
  values of the previous computation are returned.

  Note: TEST is evaluated at load time in a null lexical environment and
  must be a suitable hash table test.

  If the optional form STORE-KEY is supplied, it is evaluated after any
  evaluation of BODY and its value is used instead of KEY for storing the
  results. This way, KEY can be an object with dynamic extent (to avoid
  consing) and STORE-KEY can create a copy with indefinite extent when
  necessary."
  (once-only (key)
    `(with-hash-table-memoization
         (,key ,@(when store-key-p `(:store-key ,store-key)))
         (load-time-value
          (make-hash-table ,@(when test-p `(:test ,test))))
       ,@body)))

(defmacro with-multiple-value-memoization
    ((key &key (test nil test-p) (store-key nil store-key-p)) &body body)
  "Memoize the multiple values of BODY. If KEY has the same value (with
  respect to TEST) as some previously computed key, then BODY is not
  evaluated and the values of the previous computation are returned.

  Note: TEST is evaluated at load time in a null lexical environment and
  must be a suitable hash table test.

  If the optional form STORE-KEY is supplied, it is evaluated after any
  evaluation of BODY and its value is used instead of KEY for storing the
  results. This way, KEY can be an object with dynamic extent (to avoid
  consing) and STORE-KEY can create a copy with indefinite extent when
  necessary."
  `(values-list
    (with-memoization (,key ,@(when test-p `(:test ,test))
                            ,@(when store-key-p `(:store-key ,store-key)))
      (multiple-value-list (progn ,@body)))))

(defmacro with-hash-table-memoization
    ((key &key (store-key nil store-key-p)) hash-table &body body)
  "Memoize the value of BODY. If KEY is found in HASH-TABLE, BODY is not
  evaluated and the corresponding values are returned. Otherwise, BODY is
  evaluated and its values are first stored as the HASH-TABLE entry of KEY
  and then returned.

  If the optional form STORE-KEY is supplied, it is evaluated after any
  evaluation of BODY and its value is used instead of KEY for storing the
  results. This way, KEY can be an object with dynamic extent (to avoid
  consing) and STORE-KEY can create a copy with indefinite extent when
  necessary."
  (once-only (key hash-table)
    (with-gensyms (value present-p)
      `(multiple-value-bind (,value ,present-p)
           (gethash ,key ,hash-table)
         (if ,present-p
             ,value
             (setf (gethash ,(if store-key-p store-key key) ,hash-table)
                   (progn ,@body)))))))

(defmacro with-vector-memoization
    ((var &key (type t)) &body body)
  "Memoize the value of BODY for VAR being a relatively small integer and
   BODY returning a single value of type TYPE."
  (check-type var symbol)
  (with-gensyms (pool goal-var)
    `(let ((,pool
             (load-time-value
              (make-array 3 :fill-pointer 0
                            :element-type ',type
                            :adjustable t)))
           (,goal-var ,var))
       (declare (type array-index ,var ,goal-var)
                (type (vector ,type) ,pool))
       (loop for ,var from (fill-pointer ,pool) to ,goal-var do
         (vector-push-extend (progn ,@body) ,pool))
       (aref ,pool ,goal-var))))

(defmacro with-multiple-value-vector-memoization ((var) &body body)
  "Memoize the multiple values of BODY for VAR being a relatively small
   integer and BODY returning a single value of type TYPE."
  `(values-list
    (with-vector-memoization (,var :type list)
      (multiple-value-list (progn ,@body)))))

(defmacro define-symbol-pool (name prefix)
  (check-type prefix string)
  (check-type name symbol)
  `(progn
     (defun ,name (n)
       (with-vector-memoization (n :type symbol)
         (intern (format nil "~A~D" ,prefix n))))
     (defun ,(symbolicate name "-LIST") (length)
       (with-vector-memoization (length :type list)
         (loop for n from (1- length) downto 0
               collect (,name n))))))

