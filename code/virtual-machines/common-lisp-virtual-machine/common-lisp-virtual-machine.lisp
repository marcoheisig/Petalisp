;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class common-lisp-virtual-machine (standard-virtual-machine) ())

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
    ((virtual-machine common-lisp-virtual-machine)
     (recipe ucons))
  (with-hash-table-memoization (recipe :multiple-values nil)
      (compile-cache virtual-machine)
    (let ((code (translate-recipe-to-lambda recipe)))
      (print "Cache miss!")
      (print code)
      (finish-output)
      (compile nil code))))

(defmethod vm/execute
  ((virtual-machine common-lisp-virtual-machine)
   (kernel kernel))
  (vm/compile virtual-machine (recipe kernel)))

(defun translate-recipe-to-lambda (recipe)
  (with-ustruct-accessors (%recipe) recipe
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
         ,(translate-recipe-form expression range-steps 0)))))

(defun translate-recipe-form (uexpression range-steps depth)
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
              do ,(translate-recipe-form expression range-steps (1+ depth)))))
    (%store
     (with-ustruct-accessors (%store) uexpression
       `(setf
         ,(translate-recipe-form reference range-steps depth)
         ,(translate-recipe-form expression range-steps depth))))
    (%call
     (with-ustruct-accessors (%call) uexpression
       `(funcall
         ,operator
         ,@(iterate (for expression in-ulist expressions)
                    (collect
                        (translate-recipe-form expression range-steps depth))))))))
