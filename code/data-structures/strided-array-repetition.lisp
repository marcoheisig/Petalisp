;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-repetition (strided-array repetition) ())

(defmethod repetition ((object strided-array)
                       (space strided-array))
  (repetition object (index-space space)))

(defmethod repetition :before ((object strided-array)
                               (space strided-array-index-space))
  (assert (every (lambda (range-1 range-2)
                   (zerop (rem (size range-2) (size range-1))))
                 (ranges object)
                 (ranges space))))

(defmethod repetition ((object strided-array)
                       (space strided-array-index-space))
  (cond ((equal? (index-space object) space) object)
        (t (make-instance
            'strided-array-repetition
            :predecessors (list object)
            :element-type (element-type object)
            :ranges (ranges space)))))

(defkernel repetition-kernel (element-type input-dimension repeat?)
  (let* ((output-dimension (length repeat?))
         (input-indices
           (loop repeat input-dimension
                 collect (gensym "I")))
         (output-indices
           (loop repeat output-dimension
                 collect (gensym "O"))))
    (labels ((generate-loop (n)
               (block nil
                 (when (= n -1)
                   (return
                     `(setf (aref out ,@output-indices)
                            (aref in ,@input-indices))))
                 (let ((input-index (nth n input-indices))
                       (output-index (nth n output-indices)))
                   (if (aref repeat? n)
                       `(loop for ,output-index fixnum from 0 upto (aref ub ,n) do
                              ,(generate-loop (1- n)))
                       `(loop for ,output-index fixnum from 0 upto (aref ub ,n)
                              and ,input-index fixnum from 0 upto (aref ub ,n) do
                              ,(generate-loop (1- n))))))))
      `(lambda (in out ub)
         (declare (type (simple-array
                         ,element-type
                         ,(loop repeat input-dimension collect '*)) in)
                  (type (simple-array
                         ,element-type
                         ,(loop repeat output-dimension collect '*)) out)
                  (type (simple-array fixnum (,output-dimension)) ub)
                  (optimize (speed 3) (safety 0)))
         (let (,@(loop for i in input-indices collect `(,i 0)))
           (declare (ignorable ,@input-indices)
                    (type fixnum ,@input-indices))
           ,(generate-loop (1- output-dimension)))))))

(defmethod evaluate ((node strided-array-repetition))
  (let* ((bounds (map 'list #'size (ranges node)))
         (pred (evaluate (first (predecessors node))))
         (data (make-array bounds
                           :element-type (element-type node)))
         (ub (make-array (dimension node)
                         :element-type 'fixnum
                         :initial-contents (mapcar #'1- bounds)))
         (input-dimension (length (ranges pred)))
         (repeat? (make-array (dimension node) :initial-element t)))
    (loop for i below input-dimension do
      (setf (aref repeat? i)
            (unary-range? (aref (ranges pred) i))))
    (funcall
     (repetition-kernel (element-type node) input-dimension repeat?)
     (data pred) data ub)
    (make-instance
     'strided-array-constant
     :ranges (ranges node)
     :element-type (element-type node)
     :data data)))

