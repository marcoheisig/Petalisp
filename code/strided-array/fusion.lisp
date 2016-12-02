;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

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

(defmethod fusion ((object strided-array-index-space) &rest more-objects)
  (let ((objects (cons object more-objects)))
    (apply #'make-index-space
           (loop for i below (dimension object)
                 collect (apply #'fuse-dimension i objects)))))

(defmethod fusion ((range range) &rest more-ranges)
  (let ((ranges (cons range more-ranges)))
    (loop for range in ranges
          sum (size range) into number-of-elements
          maximize (range-end range) into end
          minimize (range-start range) into start
          finally
             (let ((step (ceiling (1+ (- end start)) number-of-elements)))
               (return (range start step end))))))

;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;;  fusion islands - specially annotated index spaces
;;; _________________________________________________________________

(define-class fusion-island (strided-array-index-space)
  (ranges-to-fuse))

(defun fuse-dimension (dimension &rest objects)
  (let* ((islands
           (apply
            #'subdivision
            (mapcar
             (lambda (object)
               (let ((ranges (ranges object)))
                 (make-instance
                  'fusion-island
                  :ranges (subseq ranges (1+ dimension))
                  :ranges-to-fuse (list (aref ranges dimension)))))
             objects)))
         (fusions
           (mapcar
            (lambda (fusion-island)
              (apply #'fusion (ranges-to-fuse fusion-island)))
            islands)))
    (assert (identical fusions :test #'equal?))
    (first fusions)))

(defmethod intersection :around ((space-1 fusion-island)
                                 (space-2 fusion-island))
  (let ((result (call-next-method)))
    (when result
      (change-class result 'fusion-island
                    :ranges-to-fuse (union (ranges-to-fuse space-1)
                                           (ranges-to-fuse space-2))))))

(defmethod difference :around ((space-1 fusion-island)
                               (space-2 fusion-island))
  (let ((result (call-next-method)))
    (mapcar
     (lambda (x)
       (change-class x 'fusion-island
                     :ranges-to-fuse (ranges-to-fuse space-1)))
     result)))

;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;;  evaluation of fusion nodes
;;; _________________________________________________________________

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
                  (optimize (speed 3) (safety 0)))
         ,(generate-loop (1- dimension))))))

(defmethod evaluate-node ((node strided-array-fusion))
  (declare (optimize debug))
  (let* ((dimension (dimension node))
         (out (make-array (map 'list #'size (ranges node))
                          :element-type (element-type node)))
         (lb (make-array dimension :element-type 'fixnum))
         (step (make-array dimension :element-type 'fixnum))
         (ub (make-array dimension :element-type 'fixnum))
         (fstart (map 'vector #'range-start (ranges node)))
         (fstep (map 'vector #'range-step (ranges node))))
    (dolist (pred (mapcar #'evaluate-node (predecessors node)))
      (let ((in (data pred))
            (pstart (map 'vector #'range-start (ranges pred)))
            (pstep (map 'vector #'range-step (ranges pred)))
            (pend (map 'vector #'range-end (ranges pred))))
        (map-into step #'/ pstep fstep)
        (map-into lb #'* (map 'vector #'- pstart fstart) fstep)
        (map-into ub #'* (map 'vector #'- pend fstart) fstep)
        (funcall
         (fusion-kernel (element-type node) dimension)
         in out lb step ub)))
    (make-instance
     'strided-array-constant
     :ranges (ranges node)
     :element-type (element-type node)
     :data out)))
