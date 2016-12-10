;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; miscellaneous utilities that would not fit elsewhere

(in-package :petalisp)

(defmacro define-class (name direct-superclasses slots &rest options)
  "Defines a class using DEFCLASS, where all slot-specifiers that consist
only of a single symbol are expanded to define a :initarg keyword and a
reader of the same name. Additionally, defines a <NAME>? predicate."
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

(defun array-map (function result array &rest more-arrays)
  (let ((n-elements (array-total-size array))
        (arrays (cons array more-arrays)))
    (loop for i below n-elements do
      (setf (row-major-aref result i)
            (apply function
                   (mapcar
                    #'(lambda (x) (row-major-aref x i))
                    arrays))))
    result))

(defun vector->list (vector)
  (loop for x across vector collect x))

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

;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;;  print useful system information
;;; _________________________________________________________________

(defun system-source-file-pathnames (system)
  (mapcar
   #'asdf:component-pathname
   (remove-if-not
    (lambda (x)
      (typep x 'asdf:source-file))
    (asdf:required-components
     (asdf:find-system system)))))

(defun print-system-statistics (system &optional (stream *standard-output*))
  (loop for pathname in (system-source-file-pathnames system)
        summing (count #\newline (read-file-into-string pathname))
          into lines-of-code
        counting pathname into files
        finally
           (format
            stream
            "The system ~a consists of ~d lines of code in ~d file~:P.~%"
            (asdf:primary-system-name system) lines-of-code files)))

(defun print-package-statistics (package &optional (stream *standard-output*))
  (loop for sym being the present-symbols of package
        counting (find-class sym nil) into classes
        when (macro-function sym)
          count it into macros
        else
          count (fboundp sym) into functions
        end
        finally
           (format
            stream
            "The package ~a defines ~d functions, ~d macros and ~d classes.~%"
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

;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;;  JIT compilation of compute intensive kernels
;;; _________________________________________________________________

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
