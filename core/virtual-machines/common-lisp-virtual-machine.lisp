;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING

(uiop:define-package :petalisp/core/virtual-machines/common-lisp-virtual-machine
  (:use :closer-common-lisp :alexandria)
  (:use
   :petalisp/utilities/all
   :petalisp/core/transformations/all
   :petalisp/core/data-structures/all
   :petalisp/core/kernelize
   :petalisp/core/virtual-machines/virtual-machine
   :petalisp/core/virtual-machines/compile-cache-mixin
   :petalisp/core/virtual-machines/default-scheduler-mixin)
  (:export
   #:common-lisp-virtual-machine))

(in-package :petalisp/core/virtual-machines/common-lisp-virtual-machine)

(define-class common-lisp-virtual-machine
    (virtual-machine default-scheduler-mixin compile-cache-mixin)
  ((memory-pool :type hash-table :initform (make-hash-table :test #'equalp))))

(defmethod vm/bind-memory
    ((virtual-machine common-lisp-virtual-machine)
     (immediate strided-array-immediate))
  (let ((array-dimensions
          (map 'list #'range-size (ranges (index-space immediate))))
        (element-type (element-type immediate)))
    (setf (storage immediate)
          (or
           (pop (gethash (cons element-type array-dimensions)
                         (memory-pool virtual-machine)))
           (make-array array-dimensions :element-type element-type)))))

(defmethod vm/free-memory
    ((virtual-machine common-lisp-virtual-machine)
     (immediate strided-array-immediate))
  (let ((array-dimensions
          (map 'list #'range-size (ranges (index-space immediate))))
        (element-type (element-type immediate)))
    (push (storage immediate)
          (gethash (cons element-type array-dimensions)
                   (memory-pool virtual-machine)))))

(defmethod vm/compile
    ((virtual-machine common-lisp-virtual-machine)
     (blueprint ucons))
  (let ((code (translate-blueprint-to-lambda blueprint)))
    (format t "~A~%" blueprint)
    (format t "~A~%" code)
    (finish-output)
    (compile nil code)))

(defmethod vm/execute
  ((virtual-machine common-lisp-virtual-machine)
   (kernel kernel))
  (funcall
   (vm/compile virtual-machine (blueprint kernel))
   (target kernel)
   (sources kernel)
   (ranges (iteration-space kernel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The kernel compiler

(define-symbol-pool index-symbol "I")
(define-symbol-pool range-symbol "RANGE-")
(define-symbol-pool source-symbol "SOURCE-")
(define-symbol-pool target-symbol "TARGET-")
(define-symbol-pool accumulator-symbol "ACCUMULATOR-")

(defmacro call (function-designator &rest arguments)
  (if (symbolp function-designator)
      `(,function-designator ,@arguments)
      `(funcall ,function-designator ,@arguments)))

(defun translate-index (index)
  (destructuring-bind (id scale offset) index
    (symbolic-+ (symbolic-* (index-symbol id) scale) offset)))

(defun translate-source-reference (memory-reference)
  (destructuring-bind (id &rest indices) memory-reference
      `(aref ,(source-symbol id) ,@(mapcar #'translate-index indices))))

(defun translate-target-reference (memory-reference)
  (destructuring-bind (id &rest indices) memory-reference
      `(aref ,(target-symbol id) ,@(mapcar #'translate-index indices))))

(defun translate-reference-info (info)
  (destructuring-bind (element-type dimension) info
    `(simple-array ,element-type ,(asterisks dimension))))

(defun translate-blueprint-to-lambda (blueprint)
  (destructuring-bind
      (range-info target-info target-reference source-infos source-references body)
      (ulist-deep-copy blueprint)
    (let ((references (map 'vector #'translate-source-reference source-references)))
      `(lambda (target sources ranges)
         (declare (type (vector t) sources)
                  (type (vector range) ranges))
         (let ((,(target-symbol 0)
                 (the ,(translate-reference-info target-info) (storage target)))
               ,@(loop for source-id from 0
                       for source-info in source-infos
                       collect
                       `(,(source-symbol source-id)
                         (the ,(translate-reference-info source-info)
                              (storage (aref sources ,source-id)))))
               ,@(loop for range-id below (length range-info)
                       collect
                       `(,(range-symbol range-id)
                         (the range (aref ranges ,range-id)))))
           ,(labels ((for (depth body)
                       `(loop for ,(index-symbol depth) of-type fixnum
                              from (range-start ,(range-symbol depth))
                                to (range-end ,(range-symbol depth))
                              by ,(third (elt range-info depth))
                              do ,body))
                     (for* (depths body)
                       (if (null depths)
                           body
                           (for (first depths)
                                (for* (rest depths) body))))
                     (reducing-for (depth binary-operator unary-operator body)
                       (let ((range-step (third (elt range-info depth))))
                         `(loop
                            with accumulator
                              = (let ((,(index-symbol depth) (range-start ,(range-symbol depth))))
                                  (declare (ignorable ,(index-symbol depth)))
                                  (call ,unary-operator ,body))
                            for ,(index-symbol depth) of-type fixnum
                            from (+ (range-start ,(range-symbol depth)) ,range-step)
                              to (range-end ,(range-symbol depth))
                            by ,range-step
                            do (setf accumulator (call ,binary-operator ,body accumulator))
                            finally (return accumulator))))
                     (translate (form depth)
                       (if (integerp form)
                           (aref references form)
                           (ecase (first form)
                             (reduce
                              (destructuring-bind (binary-operator unary-operator body) (rest form)
                                (reducing-for depth binary-operator unary-operator
                                              (translate body (1+ depth)))))
                             (funcall
                              (flet ((recurse (input) (translate input depth)))
                                `(call ,(second form) ,@(mapcar #'recurse (cddr form)))))))))
              (let ((target-dimension (second target-info)))
                (for* (iota target-dimension)
                      `(setf ,(translate-target-reference target-reference)
                             ,(translate body target-dimension))))))))))
