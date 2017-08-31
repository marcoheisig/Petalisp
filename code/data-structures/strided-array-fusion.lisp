;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-fusion (strided-array fusion) ())

(defmethod fusion ((object strided-array) &rest more-objects)
  (let ((objects (cons object more-objects)))
    (make-instance
     'strided-array-fusion
     :objects objects
     :element-type (element-type object)
     :predecessors objects
     :ranges (ranges (apply #'fusion (mapcar #'index-space objects))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  evaluation of fusion nodes

(defkernel fusion-kernel (element-type dimension)
  (let ((input-indices
          (loop repeat dimension
                collect (gensym "I")))
        (output-indices
          (loop repeat dimension
                collect (gensym "O"))))
    (labels ((generate-loop (n)
               (block nil
                 (when (= n -1)
                   (return
                     `(setf (aref out ,@output-indices)
                            (aref in ,@input-indices))))
                 `(loop for ,(nth n output-indices) fixnum
                        from (aref lb ,n)
                        upto (aref ub ,n)
                        by (aref step ,n)
                        and ,(nth n input-indices) fixnum from 0 do
                        ,(generate-loop (1- n))))))
      `(lambda (in out lb step ub)
         (declare (type (simple-array
                         ,element-type
                         ,(loop repeat dimension collect '*)) in out)
                  (type (simple-array fixnum (,dimension)) lb ub step)
                  (optimize (debug 3)))
         ,(generate-loop (1- dimension))))))

(defmethod evaluate ((node strided-array-fusion))
  (let* ((dimension (dimension node))
         (out (make-array (map 'list #'size (ranges node))
                          :element-type (element-type node)))
         (lb (make-array dimension :element-type 'fixnum))
         (step (make-array dimension :element-type 'fixnum))
         (ub (make-array dimension :element-type 'fixnum))
         (fstart (map 'vector #'range-start (ranges node)))
         (fstep (map 'vector #'range-step (ranges node))))
    (dolist (pred (mapcar #'evaluate (predecessors node)))
      (let ((in (data pred))
            (pstart (map 'vector #'range-start (ranges pred)))
            (pstep (map 'vector #'range-step (ranges pred)))
            (pend (map 'vector #'range-end (ranges pred))))
        (map-into step #'ceiling pstep fstep)
        (map-into lb #'/ (map 'vector #'- pstart fstart) fstep)
        (map-into ub #'/ (map 'vector #'- pend fstart) fstep)
        (funcall
         (fusion-kernel (element-type node) dimension)
         in out lb step ub)))
    (make-instance
     'strided-array-constant
     :ranges (ranges node)
     :element-type (element-type node)
     :data out)))

