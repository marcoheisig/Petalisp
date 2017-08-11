;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; miscellaneous utilities that would not fit elsewhere

(in-package :petalisp)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun parse-typed-lambda-list (typed-lambda-list)
   "Returns three values:
ordinary-lambda-list -- the given typed lambda list minus all the type information
type-symbols-alist -- an alist mapping from type specifiers to symbols
ftype -- a type specifier for the function with the given lambda list

A typed lambda list has the following syntax:
typed-lambda-list::= ({var | (var type)}*
                      [&optional {var | (var [init-form [type [supplied-p-parameter]]])}*]
                      [&rest {var | (var type)}]
                      [&key {var | ({var | (keyword-name var)} [init-form [type [supplied-p-parameter]]])}* [&allow-other-keys]]
                      [&aux {var | (var [init-form [type]])}*]
                      [&result type])"
   (flet ((typed-lambda-list-keyword? (x)
            (member x '(&optional &rest &key &aux &result &allow-other-keys)))
          (ordinary-lambda-list-keyword? (x)
            (member x '(:required &optional &rest &key &aux &allow-other-keys)))
          (function-type-specifier-keyword? (x)
            (member x '(:required &optional &rest &key &allow-other-keys))))
     (let ((purpose-list
             ;; e.g. the purpose-list of
             ;;    '( a          &optional  (b 5 integer)  &rest  (c string) )
             ;; is '( :required  &optional  &optional      &rest  &rest      )
             (let ((state :required))
               (mapcar
                (lambda (element)
                  (if (typed-lambda-list-keyword? element)
                      (setf state element)
                      state))
                typed-lambda-list))))
       (flet ((destructure (element purpose)
                (flet ((yield (ordinary-form var type)
                         (if (symbolp var)
                             (values ordinary-form var type)
                             (simple-program-error "Bad ~A argument: ~S" purpose element))))
                  (case purpose
                    ((&optional &key)
                     (match element
                       ((list var init-form type supplied-p-parameter)
                        (yield (list var init-form supplied-p-parameter) var type))
                       ((list var init-form type)
                        (yield (list var init-form) var type))
                       ((list var init-form)
                        (yield (list var init-form) var t))
                       ((list var)
                        (yield (list var) var t))
                       (var
                        (yield var var t))))
                    ((:required &rest)
                     (match element
                       ((list var type)
                        (yield var var type))
                       (var
                        (yield var var t))))
                    ((&aux)
                     (match element
                       ((list var init-form type)
                        (yield (list var init-form) var type))
                       ((list var init-form)
                        (yield (list var init-form) var t))
                       ((list var)
                        (yield (list var) var t))
                       (var
                        (yield var var t))))
                    ((&result)
                     (yield nil nil element))))))
         (let ((ordinary-lambda-list nil)
               (type-symbols-alist nil)
               (arg-typespec nil)
               (value-typespec '*))
           (loop :for element :in typed-lambda-list
                 :for purpose :in purpose-list :do
                   (let ((ordinary? (ordinary-lambda-list-keyword? purpose))
                         (typespec? (function-type-specifier-keyword? purpose)))
                     (if (eq purpose element)
                         (progn
                           (when ordinary? (push element ordinary-lambda-list))
                           (when typespec? (push element arg-typespec)))
                         (multiple-value-bind (ordinary-form var type)
                             (destructure element purpose)
                           (when (eq purpose '&result)
                             (if (eq value-typespec '*)
                                 (setf value-typespec type)
                                 (simple-program-error
                                  "Multiple &result types given: ~S and ~S" value-typespec type)))
                           (when ordinary?
                             (push ordinary-form ordinary-lambda-list)
                             (unless (eq type t)
                               (aif (assoc type type-symbols-alist :test #'equal)
                                    (push var (cdr it))
                                    (push (list type var) type-symbols-alist))))
                           (when typespec? (push type arg-typespec))))))
           (values
            (reverse ordinary-lambda-list)
            type-symbols-alist
            `(function ,(reverse arg-typespec) ,value-typespec))))))))

(defmacro define-function (&whole whole name typed-lambda-list &body body)
  "DEFUN on steroids.

Examples:
 (define-function f (x) 0)
 => (progn
      (declaim (inline f))
      (defun f (x) 0)
      (declaim (notinline f)))

 (define-function (f :inline) (x) 0)
 => (progn
      (declaim (inline f))
      (defun f (x) 0))

 (define-function (f :volatile) (x) 0)
 => (progn
      (declaim (notinline f))
      (defun f (x) 0))

 (define-function f ((x string) &result integer) 0)
 => (progn
      (declaim (inline f)
               (ftype (function (string) integer) f))
      (defun f (x)
        (declare (type string x))
        0)
      (declaim (notinline f)))

 (define-function f ((g (function (string) string)))
   (g \"I thought this was a Lisp-2\"))
 => (progn
      (declaim (inline f)
               (ftype (function ((function (string) string)) *)))
      (defun f (x)
        (declare (type (function (string) string) g))
        (flet ((g (#:GENSYM)
                 (funcall g #:gensym)))
          (declare (inline g))
          (g \"I thought this was a Lisp-2\")))
      (declaim (notinline f)))"
  (multiple-value-bind (name option)
      (match name
        ((or (list 'setf name option)
             (list 'setf name))
         (if #1= (and (symbolp name)
                      (member option '(nil :volatile :inline)))
             (values `(setf ,name) option)
             (fail)))
        ((or (list name option)
             (list name)
             name)
         (if #1#
             (values name option)
             (fail)))
        (_ (simple-program-error
            "Invalid function specifier:~%  ~S" name)))
    (multiple-value-bind (ordinary-lambda-list type-symbols-alist ftype)
        (parse-typed-lambda-list typed-lambda-list)
      (multiple-value-bind (body-forms declarations documentation)
          (parse-body body :documentation t :whole whole)
        (let ((flet-bindings
                (loop :for (type-specifier . symbols) :in type-symbols-alist
                      :when (subtypep type-specifier 'function)
                        :append
                        (loop :for symbol :in symbols :collect
                              `(,symbol (&rest args) (apply ,symbol args))))))
          (let ((fdefinition `(defun ,name ,ordinary-lambda-list
                                ,@(when documentation `(,documentation))
                                ,@(when type-symbols-alist
                                    `((declare
                                       ,@(loop for entry in type-symbols-alist
                                               collect `(type ,@entry)))))
                                ,@declarations
                                ,@(if flet-bindings
                                      `((flet ,flet-bindings
                                          (declare (inline ,@(mapcar #'first flet-bindings)))
                                          ,@body-forms))
                                      body-forms))))
            (case option
              (nil `(progn
                      (declaim (inline ,name)
                               (ftype ,ftype ,name))
                      ,fdefinition
                      (declaim (notinline ,name))))
              (:inline `(progn
                          (declaim (inline ,name)
                                   (ftype ,ftype ,name))
                          ,fdefinition))
              (:volatile `(progn
                            (declaim (notinline ,name))
                            ,fdefinition)))))))))

(defmacro λ (&rest symbols-and-expr)
  "A shorthand notation for lambda expressions, provided your Lisp
implementation is able to treat λ as a character.

Examples:
 (λ x x) -> (lambda (x) x)
 (λ x y (+ x y)) -> (lambda (x y) (+ x y))"
  `(lambda ,(butlast symbols-and-expr)
     ,@(last symbols-and-expr)))

(defmacro define-class (class-name superclass-names slot-specifiers &rest class-options)
  "Defines a class using DEFCLASS, where all slot-specifiers that consist
only of a single symbol are expanded to define a :initarg keyword and a
reader of the same name. Additionally, defines a <NAME>? predicate."
  (flet ((extend-slot-specifier (slot-specifier)
           (destructuring-bind (slot-name &rest slot-options)
               (ensure-list slot-specifier)
             (let ((plist (copy-list slot-options)))
               (unless (or (getf plist :reader)
                           (getf plist :accessor))
                 (setf (getf plist :reader) slot-name))
               (unless (getf plist :initarg (ensure-keyword slot-name))))
             `(,slot-name ,@plist))))
    `(progn
       (defclass ,class-name ,superclass-names
         ,(mapcar #'extend-slot-specifier slot-specifiers)
         ,@class-options)
       (defun ,(intern (concatenate 'string (symbol-name name) "?")) (x)
         (typep x ',name)))))

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

(defun identical? (sequence &key (test #'eql) (key #'identity))
  "Returns T if the KEY of all elements of SEQUENCE are equal with respect
to TEST. Otherwise, returns false."
  (declare (function test key))
  (etypecase sequence
    (list
     (or (null sequence)
         (loop :for element :in (cdr sequence)
               :with reference := (funcall key (car sequence))
               :always
               (funcall test (funcall key element) reference))))
    (simple-vector
     #1=
     (let ((n (length sequence)))
       (or (zerop n)
           (loop :for i fixnum :from 1 :below n
                 :with reference := (funcall key (elt sequence 0))
                 :always
                 (funcall test (funcall key (elt sequence i)) reference)))))
    (sequence #1#)))

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
