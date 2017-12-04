;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class common-lisp-virtual-machine
    (virtual-machine default-scheduler-mixin compile-cache-mixin)
  ((memory-pool :type hash-table :initform (make-hash-table :test #'equalp))))

(defmethod vm/bind-memory
    ((virtual-machine common-lisp-virtual-machine)
     (immediate strided-array-immediate))
  (let ((array-dimensions
          (map 'list #'size (ranges (index-space immediate))))
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
          (map 'list #'size (ranges (index-space immediate))))
        (element-type (element-type immediate)))
    (push (storage immediate)
          (gethash (cons element-type array-dimensions)
                   (memory-pool virtual-machine)))))

(define-symbol-pool range-start-symbol "RANGE-START-")
(define-symbol-pool range-end-symbol "RANGE-END-")
(define-symbol-pool storage-symbol "STORAGE-")

(defmethod vm/compile
    ((virtual-machine common-lisp-virtual-machine) (blueprint ucons))
  (let ((code nil #+nil(translate-blueprint-to-lambda blueprint)))
    (format t "~A~%" blueprint)
    (format t "~A~%" code)
    (finish-output)
    ;;(compile nil code)
    (lambda (&rest rest))))

(defmethod vm/execute
  ((virtual-machine common-lisp-virtual-machine)
   (kernel kernel))
  (let (arguments)
    (loop for range across (ranges (iteration-space kernel)) do
      (push (range-start range) arguments)
      (push (range-end range) arguments))
    (push (storage (target kernel)) arguments)
    (loop for source across (sources kernel) do
      (push (storage source) arguments))
    (apply
     (vm/compile virtual-machine (blueprint kernel))
     (nreverse arguments))))

(defun translate-blueprint-to-lambda (blueprint)
  (destructure-ulist (range-info memory-references target-info source-info body)
      blueprint
    (let ((range-steps (fvector))
          declarations
          lambda-list)
      (iterate (for triple in-ulist range-info)
               (for index from 0)
               (fvector-push (ucaddr triple) range-steps)
               (push (range-start-symbol index) lambda-list)
               (push (range-end-symbol index) lambda-list))
      (iterate (for element-type in-ulist storage-info)
               (for index from 0)
               (push (storage-symbol index) lambda-list)
               (push `(type (simple-array ,element-type)
                            ,(storage-symbol index))
                     declarations))
      `(lambda ,(nreverse lambda-list)
         (declare ,@declarations)
         ,(translate-blueprint-form body range-steps 0)))))

(defun translate-blueprint-form (uexpression range-steps depth)
  (ecase (ucar uexpression)
    (%reference
     (with-ustruct-accessors (%reference) uexpression
       `(aref ,(storage-symbol storage)
              ,@(iterate (for triple in-ulist indices)
                         (collect
                             `(+ (* ,(index-symbol (ucar triple))
                                    ,(ucadr triple))
                                 ,(ucaddr triple)))))))
    (%for
     (with-ustruct-accessors (%for) uexpression
       `(loop for ,(index-symbol depth)
              of-type fixnum
              from ,(range-start-symbol range)
              by ,(aref range-steps depth)
                to ,(range-end-symbol range)
              do ,(translate-blueprint-form expression range-steps (1+ depth)))))
    (%store
     (with-ustruct-accessors (%store) uexpression
       `(setf
         ,(translate-blueprint-form reference range-steps depth)
         ,(translate-blueprint-form expression range-steps depth))))
    (%call
     (with-ustruct-accessors (%call) uexpression
       `(funcall
         ,operator
         ,@(iterate (for expression in-ulist expressions)
                    (collect
                        (translate-blueprint-form expression range-steps depth))))))))
