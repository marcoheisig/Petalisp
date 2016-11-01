;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-fusion (strided-array fusion) ())

;;; (fusion #i((1 2 3) (1 2 3)) #i((2 2 4) (1 2 3)) #i((1 2 3) (2 2 4)) #i((2 2 4) (2 2 4)))
(defmethod fusion ((object strided-array) &rest more-objects)
  (let ((objects (list* object more-objects))
        (dimension (dimension object)))
    (assert (identical objects :key #'dimension))
    (make-instance
     'strided-array-fusion
     :objects objects
     :ranges
     (let ((array (make-array dimension)))
       (loop for i below dimension do
         (setf (aref array i)
               (apply #'fuse-dimension i objects)))
       array))))

(defmethod fusion ((range range) &rest more-ranges)
  (let* ((ranges (list* range more-ranges))
         (fusion
           (loop for range in (list* range more-ranges)
                 sum (size range) into number-of-elements
                 maximize (range-end range) into end
                 minimize (range-start range) into start
                 finally
                    (let ((step (ceiling (1+ (- end start))
                                         number-of-elements)))
                      (return (range start step end))))))
    (assert (every (lambda (x) (subspace? x fusion)) ranges))
    fusion))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; fusion islands - index spaces that keep track of the ranges that must
;;; be fused in the next higher dimension

(define-class fusion-island (strided-array-index-space)
  (ranges-to-fuse))

;;; (fuse-dimension 0 #i((1 2 3) (1 2 3)) #i((2 2 4) (1 2 3)) #i((1 2 3) (2 2 4)) #i((2 2 4) (2 2 4)))
(defun fuse-dimension (dimension &rest objects)
  (let* ((islands
           (apply
            #'subdivide
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

;;; (subdivide #i((1 1 4)) #i((1 2 5)))
(defun subdivide (&rest fusion-islands)
  (let (src dst intersectionp)
    (dolist (a fusion-islands src)
      (dolist (b src)
        (let ((i (intersection a b)))
          (cond
            (i (setf intersectionp t)
               (mapc
                (lambda (x) (push x dst))
                (let ((tmp
                        `(,i ,@(difference b a)
                             ,@(difference a b))))
                  tmp)))
            (t (push b dst)))))
      (unless intersectionp (push a dst))
      (psetf intersectionp nil dst () src dst))))

(defmethod intersection :around ((space-1 fusion-island)
                                 (space-2 fusion-island))
  (let ((result (call-next-method)))
    (when result
      (change-class result 'fusion-island
                    :ranges-to-fuse (union (ranges-to-fuse space-1)
                                           (ranges-to-fuse space-2))))))

(defmethod difference :around ((space-1 fusion-island)
                               (space-2 fusion-island))
  (mapcar
   (lambda (result)
     (change-class result 'fusion-island
                   :ranges-to-fuse (ranges-to-fuse space-1)))
   (call-next-method)))
