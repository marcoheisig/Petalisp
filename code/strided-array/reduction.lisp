;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-reduction (strided-array reduction) ())

(defun all-but-last (sequence)
  (subseq sequence 0 (1- (length sequence))))

(defmethod reduction ((operator function) (object strided-array))
  (make-instance
   'strided-array-reduction
   :operator operator
   :element-type (element-type object)
   :predecessors (list object)
   :ranges (all-but-last (ranges object))))

(defmethod reduction ((operator function) (object strided-array-constant))
  (if (<= (size object) *constant-fold-threshold*)
      (evaluate-node (call-next-method)) ; constant folding
      (call-next-method)))

(defkernel reduction-kernel (function element-type output-dimension)
  (let* ((input-dimension (1+ output-dimension))
         (input-indices
           (loop repeat input-dimension
                 collect (gensym "I")))
         (output-indices
           (loop repeat output-dimension
                 collect (gensym "O"))))
    (labels ((generate-loop (n)
               (block nil
                 (when (= n 0)
                   (return
                     `(let ((acc (aref in 0 ,@(cdr input-indices))))
                        (loop for ,(first input-indices) fixnum
                              from 1 below (aref ub ,n) do
                                (setf acc (funcall ,function acc
                                                   (aref in ,@input-indices))))
                        (setf (aref out ,@output-indices) acc))))
                 `(loop for ,(nth (1- n) output-indices) fixnum
                        from 0 below (aref ub ,n)
                        and ,(nth n input-indices) fixnum from 0 do
                        ,(generate-loop (1- n))))))
      `(lambda (in out ub)
         (declare (type (simple-array
                         ,element-type
                         ,(loop repeat input-dimension collect '*)) in)
                  (type (simple-array
                         ,element-type
                         ,(loop repeat output-dimension collect '*)) out)
                  (type (simple-array fixnum (,input-dimension)) ub)
                  #+nil(optimize (speed 3) (safety 0)))
         ,(generate-loop output-dimension)))))

(defmethod evaluate-node ((node strided-array-reduction))
  (let* ((op (operator node))
         (pred (evaluate-node (first (predecessors node))))
         (ub (make-array (dimension pred)
                         :element-type 'fixnum
                         :initial-contents (map 'list #'size (ranges pred))))
         (result (make-array
                  (map 'list #'size (ranges node))
                  :element-type (element-type node))))
    (funcall
     (reduction-kernel op (element-type node) (dimension node))
     (data pred) result ub)
    (make-instance
     'strided-array-constant
     :data result
     :element-type (element-type node)
     :ranges (ranges node))))
