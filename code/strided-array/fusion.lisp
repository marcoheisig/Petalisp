;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class strided-array-fusion (strided-array fusion) ())

;;; (fusion #i((1 2 3) (1 2 3)) #i((2 2 4) (1 2 3)) #i((1 2 3) (2 2 4)) #i((2 2 4) (2 2 4)))
(defmethod fusion ((object strided-array) &rest more-objects)
  (let ((objects (list* object more-objects))
        (dimension (dimension object)))
    (assert (apply #'= (mapcar #'dimension objects)))
    (make-instance
     'strided-array-fusion
     :objects objects
     :ranges
     (loop for d from 1 upto dimension
          collect
          (let ((islands (apply #'subdivide (apply #'islandize d objects))))
            (loop for island in (cdr islands)
                  with range = (apply #'fuse (ranges-to-fuse (car islands))) do
                    (assert (equalp range (apply #'fuse (ranges-to-fuse island))))
                  finally (return range)))))))

(defmethod fusion ((range range) &rest more-ranges)
  ;; note that this method assumes that the ranges are disjoint
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
    (assert (every
             (lambda (range)
               (equalp range
                       (intersection range fusion)))
             ranges))
    fusion))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; fusion islands - index spaces that keep track of the ranges that must
;;; be fused in the next higher dimension

;;; (islandize 1 #i((1 2 3) (1 2 3)) #i((2 2 4) (1 2 3)) #i((1 2 3) (2 2 4)) #i((2 2 4) (2 2 4)))
(defun islandize (dimension &rest objects)
  (mapcar
   (lambda (x)
     (destructuring-bind (range . ranges)
         (nthcdr (1- dimension) (ranges x))
       (make-instance
        'fusion-island
        :ranges ranges
        :ranges-to-fuse (list range))))
   objects))

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

(define-class fusion-island (strided-array-index-space)
  (ranges-to-fuse))

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
