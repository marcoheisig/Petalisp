;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; testing

(in-package :petalisp)

(defmacro test (name &body body)
  "A thin wrapper around FIVEAM:TEST that automatically uses the name of
the current package (interned in the current package) as test suite."
  `(progn
     (fiveam:in-suite* ,(intern (package-name *package*)))
     (fiveam:test ,name ,@body)))

(macrolet
    ((float-generator (type)
       `(defun ,(symbolicate type "-GENERATOR")
            (&optional
               (infimum ,(coerce -2 type))
               (supremum ,(coerce 2 type)))
          (assert (< infimum supremum))
          (let ((infimum (coerce infimum ',type))
                (supremum (coerce supremum ',type)))
            (let ((middle (+ (/ supremum 2) (/ infimum 2)))
                  (half   (- (/ supremum 2) (/ infimum 2))))
              (if (zerop half)
                  (constantly middle)
                  (lambda ()
                    (let ((offset (random half)))
                      (if (zerop (random 2))
                          (- middle offset)
                          (+ middle offset))))))))))
  (float-generator short-float)
  (float-generator single-float)
  (float-generator double-float)
  (float-generator long-float))

(defun integer-generator (&optional
                            (minimum (floor most-negative-fixnum 4/5))
                            (maximum (ceiling most-positive-fixnum 4/5)))
  (check-type minimum integer)
  (check-type maximum integer)
  (assert (<= minimum maximum))
  (lambda ()
    (+ minimum (random (1+ (- maximum minimum))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; miscellaneous utilities that would not fit elsewhere

(defmacro λ (&rest symbols-and-expr)
  "A shorthand notation for lambda expressions, provided your Lisp
implementation is able to treat λ as a character.

Examples:
 (λ x x) -> (lambda (x) x)
 (λ x y (+ x y)) -> (lambda (x y) (+ x y))"
  `(lambda ,(butlast symbols-and-expr)
     ,@(last symbols-and-expr)))

(defmacro with-unsafe-optimizations* (&body body)
  "Optimize the heck out of BODY. Use with caution!"
  `(locally
       (declare
        (optimize (speed 3) (space 0) (debug 0) (safety 0) (compilation-speed 0)))
     ,@body))

(defmacro with-unsafe-optimizations (&body body)
  "Optimize the heck out of BODY. Use with caution!

To preserve sanity, compiler efficiency hints are disabled by default. Use
WITH-UNSAFE-OPTIMIZATIONS* to see these hints."
  `(locally (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
     (with-unsafe-optimizations* ,@body)))

(defmacro define-class (class-name superclass-names slot-specifiers &rest class-options)
  "Defines a class using DEFCLASS, but defaulting to a :READER of
SLOT-NAME and. Additionally, defines a <NAME>? predicate."
  (flet ((extend-slot-specifier (slot-specifier)
           (destructuring-bind (slot-name &rest plist)
               (ensure-list slot-specifier)
             (let ((initarg (unless (getf plist :initarg)
                              (list :initarg (make-keyword slot-name))))
                   (reader (unless (or (getf plist :reader)
                                       (getf plist :accessor)
                                       (getf plist :writer))
                             (list :reader slot-name))))
               `(,slot-name ,@initarg ,@reader ,@plist)))))
    `(progn
       (defclass ,class-name ,superclass-names
         ,(mapcar #'extend-slot-specifier slot-specifiers)
         ,@class-options)
       (defun ,(symbolicate class-name "?") (x)
         (typep x ',class-name)))))

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

(test extended-euclid
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

(test identical
  (is-true (identical '(1 1 1)))
  (is-true (identical #(1 1.0 1/1) :test #'=))
  (is-true (identical '(1 2.0 6/2 4d0) :key #'numberp :test #'eq))
  (is-true (identical "aaaAaa" :test #'char= :key #'char-upcase)))

(defmacro zapf (place expr)
  (multiple-value-bind
        (temps exprs stores store-expr access-expr)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores)
              (let ((% ,access-expr))
                ,expr)))
       ,store-expr)))

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

(test array-map
  (is (equalp
       #2a((1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4))
       (array-map
        #'+
        (make-array '(4 4))
        #2a((1 1 1 1) (1 1 1 1) (1 1 1 1) (1 1 1 1))
        #2a((0 1 2 3) (0 1 2 3) (0 1 2 3) (0 1 2 3))))))

(defun vector->list (vector)
  (loop :for x :across vector :collect x))

(defun forever (value)
  "Make a one element circular list with a CAR of VALUE."
  (let ((x (list value)))
    (setf (cdr x) x)))

(defmacro defalias (alias fname)
  "Define ALIAS to be an alias to the function FNAME."
  `(progn
     (eval-when (:compile-toplevel)
       (defun ,alias (&rest args)
         (declare (ignore args))))
     (eval-when (:load-toplevel :execute)
       (setf (fdefinition ',alias)
             #',fname)
       (setf (documentation ',alias 'function)
             (documentation #',fname 'function)))))

(defun list-of-symbols (length)
  (loop :for i :below length
        :with abc := #(a b c d e f g h i j k l m n o p q r s t. u v w x y z)
        :collect (if (< i (length abc))
                     (aref abc i)
                     (symbolicate "VAR" i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  print useful system information

(defun system-source-file-pathnames (system)
  (mapcar
   #'asdf:component-pathname
   (remove-if-not
    (lambda (x) (typep x 'asdf:source-file))
    (asdf:required-components
     (asdf:find-system system)))))

(defun print-system-statistics (system &optional (stream *standard-output*))
  (loop :for pathname :in (system-source-file-pathnames system)
        :summing (count #\newline (read-file-into-string pathname))
          :into lines-of-code
        :counting pathname :into files
        :finally
           (format
            stream
            "The system ~s consists of ~d lines of code in ~d file~:P.~%"
            (asdf:primary-system-name system) lines-of-code files)))

(defun print-package-statistics (package &optional (stream *standard-output*))
  (loop :for sym :being :the :present-symbols :of package
        :counting (find-class sym nil) :into classes
        :when (macro-function sym)
          :count :it :into macros
        :else
          :count (fboundp sym) :into functions
        :end
        :finally
           (format
            stream
            "The package ~s defines ~d functions, ~d macros and ~d classes.~%"
            (package-name (find-package package)) functions macros classes)))

(defun print-platform-information (&optional (stream *standard-output*))
  (format stream "Implementation: ~:[Something weird~;~:*~a~]"
          (lisp-implementation-type))
  (format stream "~@[ ~a~]~%"
          (lisp-implementation-version))
  (format stream "Machine: ~:[a strange system~;~:*~a~]"
          (machine-type))
  (format stream "~@[ ~a~]~%"
          (machine-version)))

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
