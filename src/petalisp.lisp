;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Petalisp objects

(in-package :petalisp)

(defclass petalisp-object () ())

(defclass petalisp-operation (petalisp-object)
  ((%index-space :initarg :index-space :accessor index-space)
   (%element-type :initarg :element-type :accessor element-type)))

(defclass unary-operation (petalisp-operation) ())

(defclass binary-operation (petalisp-operation) ())

(defmacro define-operator (name args)
  (let ((fsym (intern
               (concatenate 'string "*" (symbol-name name))
               :petalisp))
        (superclass (ecase (length args)
                      (1 `unary-operation)
                      (2 `binary-operation)))
        (initargs (ecase (length args)
                    (1 `(:arg ,(first args)))
                    (2 `(:arg1 ,(first args) :arg2 ,(second args))))))
    `(progn
       (defclass ,name (,superclass) ())
       (defun ,fsym ,args
         (make-instance ,`',name ,@initargs)))))

;; *constant
;; *reduce
;; *scan
;; *apply
;; *broadcast
;; *select
;; *fuse
;; *permute
;; *index
;; *member

(define-operation + (a b))

(defmacro define-petalisp-operation (name gf-lambda-list)
  (let* ((args (remove-if
                (lambda (x) (char-equal #\& (aref (symbol-name x) 0)))
                gf-lambda-list))
         (table
           (loop for arg in args
                 collect
                 `(,arg
                   ,(intern (concatenate 'string "%" (symbol-name arg)))
                   ,(intern (symbol-name arg) :keyword)))))
    `(progn
       (defclass ,name (petalisp-array)
         ,(loop for (reader slot-name initarg) in table
                collect
                `(,slot-name :initarg ,initarg :reader ,reader)))
       (defgeneric ,name ,gf-lambda-list)
       (defmethod ,name ,gf-lambda-list
         (let ((args (list ',name ,@(loop for (value _ key) in table
                                         append `(,key ,value)))))
           (or (apply #'find-instance args)
               (apply #'make-instance args)))))))

(define-petalisp-operation constant (index-space value))

(define-petalisp-operation reduction (op dim arg))

(define-petalisp-operation selection (subspace arg))

(define-petalisp-operation fusion (&rest args))

(define-petalisp-operation unary-application (op arg))

(define-petalisp-operation binary-application (op arg1 arg2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constant folding
;;;
;;; Constant folding in Petalisp means that identical operations on
;;; identical objects should be EQ.

;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Normalization - ensure that petalisp objects are well formed

(defmethod normalize ((instance constant))
  (with-accessors ((element-type element-type) (value value)) instance
    (setf element-type (type-of value))))

(defmethod normalize ((instance reduction))
  (with-accessors ((element-type element-type) (dim dim) (arg arg)
                   (index-space index-space)) instance
    (setf element-type (type-of arg))
    (setf index-space (index-space-reduction dim index-space))))
