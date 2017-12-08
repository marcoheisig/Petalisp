;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp-internals)

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

(defvar *memoization-tables* (make-weak-hash-table :test #'eq :weakness :key)
  "A mapping from packages to sets of of all implicitly created memoization
tables in that package.")

(defun package-memoization-tables (&optional (package *package*))
  "Return the set of memoization tables used by PACKAGE. The set is
represented as a NIL valued hash table whose (weak) keys are individual
memoization tables. This permits the garbage collector to reclaim unused
memoization tables, e.g. after the redefinition of a function."
  (or (gethash package *memoization-tables*)
      (setf (gethash package *memoization-tables*)
            (make-weak-hash-table :test #'eq :weakness :key))))

(defun clear-memoization-tables (&optional (package *package*))
  "Clear all memoization tables in PACKAGE."
  (loop for memoization-table being each hash-key of (package-memoization-tables package)
        when (hash-table-p memoization-table)
          do (clrhash memoization-table)))

(defmacro with-memoization
    ((key &key (test #'eql) (store-key nil store-key-p) (multiple-values t))
     &body body)
  "Memoize the values of BODY. If KEY has the same value (with respect to
  TEST) as some previously computed key, then BODY is not evaluated and the
  values of the previous computation are returned.

  Note: TEST is evaluated at load time in a null lexical environment and
  must be a suitable hash table test.

  If the optional form STORE-KEY is supplied, it is evaluated after any
  evaluation of BODY and its value is used instead of KEY for storing the
  results. This way, KEY can be an object with dynamic extent (to avoid
  consing) and STORE-KEY can create a copy with indefinite extent when
  necessary."
  (assert (typep test '(or symbol (cons (eql function) (cons symbol null))))
          (test) "TEST must be a valid function designator.")
  (check-type multiple-values boolean)
  (once-only (key)
    (with-gensyms (hash-table)
      `(with-hash-table-memoization
           (,key ,@(when store-key-p (list :store-key store-key))
                 :multiple-values ,multiple-values)
           (load-time-value
            (let ((,hash-table (make-hash-table :test ,test)))
              (setf (gethash ,hash-table (package-memoization-tables)) nil)
              ,hash-table))
         ,@body))))

(defmacro with-hash-table-memoization
    ((key &key (store-key nil store-key-p) (multiple-values t))
     hash-table &body body)
  "Memoize the values of BODY. If KEY is found in HASH-TABLE, BODY is not
  evaluated and the corresponding values are returned. Otherwise, BODY is
  evaluated and its values are first stored as the HASH-TABLE entry of KEY
  and then returned.

  If the optional form STORE-KEY is supplied, it is evaluated after any
  evaluation of BODY and its value is used instead of KEY for storing the
  results. This way, KEY can be an object with dynamic extent (to avoid
  consing) and STORE-KEY can create a copy with indefinite extent when
  necessary."
  (check-type multiple-values boolean)
  (once-only (key hash-table)
    (let ((place `(gethash ,(if store-key-p store-key key) ,hash-table)))
      (if multiple-values
          (with-gensyms (list-of-values)
            `(values-list
              (with-generic-memoization (gethash ,key ,hash-table)
                (let ((,list-of-values (multiple-value-list (progn ,@body))))
                  (setf ,place ,list-of-values)))))
          (with-gensyms (value)
            `(with-generic-memoization (gethash ,key ,hash-table)
               (let ((,value (progn ,@body)))
                 (setf ,place ,value))))))))

(defmacro with-generic-memoization
    (lookup-form &body body)
  "When the second value of LOOKUP-FORM is true, return its first value.
   Otherwise, evaluate BODY and return its value."
  (with-gensyms (value present-p)
    `(multiple-value-bind (,value ,present-p) ,lookup-form
       (if ,present-p ,value (progn ,@body)))))

;;; This macro is much more specialized than the previous ones. It does not
;;; handle multiple values and permits only a single integer as key. On the
;;; other hand, unless key becomes large, it offers blazingly fast
;;; memoization.
(defmacro with-vector-memoization
    ((var &key (type t)) &body body)
  "Memoize the value of BODY for VAR being a relatively small integer and
   BODY returning a single value of type TYPE."
  (check-type var symbol)
  (with-gensyms (pool limit)
    `(let ((,pool
             (load-time-value
              (make-array 0 :fill-pointer 0 :element-type ',type)))
           (,limit ,var))
       (declare (type array-index ,var ,limit)
                (type (vector ,type) ,pool))
       (iterate
         (for ,var from (fill-pointer ,pool) to ,limit)
         (vector-push-extend (progn ,@body) ,pool))
       (aref ,pool ,limit))))

