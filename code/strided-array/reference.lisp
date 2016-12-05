;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-reference (strided-array reference) ())

(defmethod reference ((object strided-array)
                      (space strided-array-index-space)
                      (transformation transformation))
  (let ((target-space (transform space transformation)))
    (make-instance
     'strided-array-reference
     :predecessors (list object)
     :ranges (ranges target-space)
     :element-type (element-type object)
     :transformation transformation)))

(defmethod reference ((object strided-array)
                      (space strided-array-index-space)
                      (transformation identity-transformation))
  (let ((target-space (transform space transformation)))
    (if (equal? (index-space object) target-space)
        object
        (call-next-method))))

(defmethod reference ((object strided-array-reference)
                      (space strided-array-index-space)
                      (t2 transformation))
  "Fold references to other references."
  (let ((target-space (transform space t2))
        (transformation (compose t2 (transformation object))))
    (reference
     (first (predecessors object))
     (transform target-space (invert transformation))
     transformation)))

(defmethod reference ((fusion strided-array-fusion)
                      (space strided-array-index-space)
                      (transformation transformation))
  "Make references to fusions reference the inputs of the fusion instead."
  (aif (find space (predecessors fusion) :test #'equal? :key #'index-space)
       (reference it space transformation)
       (call-next-method)))

(defkernel reference-kernel (element-type input-dimension permutation direction)
  (let* ((output-dimension (length permutation))
         (input-indices
           (loop repeat input-dimension
                 collect (gensym "I")))
         (output-indices
           (loop repeat output-dimension
                 collect (gensym "O")))
         (dim (length permutation)))
    (labels ((generate-loop (n)
               (block nil
                 (when (= n -1)
                   (return
                     `(setf (aref out ,@output-indices)
                            (aref in ,@input-indices))))
                 (let ((inpos (aref permutation n))
                       (output-index (nth n output-indices)))
                   (when (not inpos)
                     (return
                       `(let ((,output-index 0))
                          ,(generate-loop (1- n)))))
                   (let ((input-index (nth inpos input-indices)))
                     (if (aref direction n)
                         `(loop for ,input-index fixnum
                                from (aref lb ,inpos)
                                upto (aref ub ,inpos)
                                and ,output-index fixnum from 0 do
                                ,(generate-loop (1- n)))
                         `(loop for ,input-index fixnum
                                from (aref ub ,inpos)
                                downto (aref lb ,inpos)
                                and ,output-index fixnum from 0 do
                                ,(generate-loop (1- n)))))))))
      `(lambda (in out lb ub)
         (declare (type (simple-array
                         ,element-type
                         ,(loop repeat input-dimension collect '*)) in)
                  (type (simple-array
                         ,element-type
                         ,(loop repeat output-dimension collect '*)) out)
                  (type (simple-array fixnum (,input-dimension)) lb ub)
                  (ignorable lb ub)
                  (optimize (speed 3) (safety 0)))
         (let (,@(loop for i in input-indices collect `(,i 0)))
           (declare (ignorable ,@input-indices)
                    (type fixnum ,@input-indices))
           ,(generate-loop (1- dim)))))))

(defmethod evaluate ((node strided-array-reference))
  (let* ((predecessor (evaluate (first (predecessors node))))
         (transformation (transformation node))
         (input-ranges
           (ranges
            (transform
             (index-space node)
             (invert transformation))))
         (direction (make-array (output-dimension transformation)))
         (out (make-array (map 'list #'size (ranges node))
                          :element-type (element-type node)))
         (lb (make-array (input-dimension transformation)
                         :element-type 'fixnum))
         (ub (make-array (input-dimension transformation)
                         :element-type 'fixnum)))
    ;; determine the DIRECTION of each array access
    (loop for i below (output-dimension transformation)
          with c = (affine-coefficients transformation) do
            (setf (aref direction i) (plusp (aref c i 0))))
    ;; determine LB and UB, the bounds of the input data to be read
    (loop for irange across input-ranges
          and prange across (ranges predecessor)
          and i from 0 do
            (setf (aref lb i)
                  (/ (- (range-start irange) (range-start prange))
                     (range-step irange)))
            (setf (aref ub i)
                  (/ (- (range-end irange) (range-start prange))
                     (range-step irange))))
    (funcall
     (reference-kernel
      (element-type node)
      (input-dimension transformation)
      (permutation transformation)
      direction)
     (data predecessor) out lb ub)
    (make-instance
     'strided-array-constant
     :ranges (ranges node)
     :element-type (element-type node)
     :data out)))
