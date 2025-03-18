(in-package #:petalisp.benchmarks)

(defun lazy-stencil (u weight-offsets-list)
  "Apply to U the stencil with the supplied weights and offsets.  Each
weight-offsets entry should have the form (weight . offsets)."
  (with-lazy-arrays (u)
    (let* ((rank (if (null weight-offsets-list)
                     0
                     (1- (length (first weight-offsets-list)))))
           (alist '())
           (omin (make-list rank :initial-element 0))
           (omax (make-list rank :initial-element 0)))
      (loop for (weight . offsets) in weight-offsets-list do
        (assert (= (length offsets) rank))
        ;; Group offsets with the same weight.
        (push offsets (alexandria:assoc-value alist weight))
        ;; Determine the minimum and maximum offset.
        (setf omin (mapcar #'min omin offsets))
        (setf omax (mapcar #'max omax offsets)))
      ;; Determine the interior.
      (let* ((left (mapcar #'- omin))
             (right omax)
             (interior
               (funcall (apply #'peeler (mapcar #'list left right))
                        (lazy-array-shape u))))
        ;; Apply the stencil.
        (lazy-overwrite-and-harmonize
         u
         (apply
          'lazy #'+
          (loop
            for (w . list-of-offsets) in alist
            collect
            (lazy #'*
             w
             (apply
              #'lazy #'+
              (loop
                for offsets in list-of-offsets
                collect
                (lazy-reshape u
                 (make-transformation
                  :input-rank rank
                  ;; Flip the sign of each offset, because we are first shifting
                  ;; and then selecting the interior.
                  :offsets (map 'vector #'- offsets))
                 interior)))))))))))

(defun stencil-bench (nbytes nreps weight-offsets-list)
  (assert (not (null weight-offsets-list)))
  (let* ((rank (1- (length (first weight-offsets-list))))
         (nwords (ceiling nbytes 8))
         (dim (ceiling (expt (/ nwords 2) (/ rank))))
         (dims (make-list rank :initial-element dim))
         (src (make-array dims :element-type 'double-float :initial-element 1d0))
         (dst (make-array dims :element-type 'double-float :initial-element 1d0))
         (x (make-unknown :shape (petalisp.core:array-shape src)
                          :element-type 'double-float))
         (r (let ((v x))
              (loop repeat nreps do
                (setf v (lazy-stencil v weight-offsets-list)))
              v))
         (flops (* 2 (flopcount r)))
         (bytes (apply #'* 8 2 dims))
         (ev (evaluator (list x) (list r))))
    (assert (<= nbytes bytes))
    (values
     (lambda ()
       (funcall ev dst src)
       (funcall ev src dst))
     flops
     (/ flops (* 2 nreps bytes)))))

(defbenchmark stencil-jacobi-2d (nbytes)
  (stencil-bench
   nbytes
   10
   '((+0.25d0  0 +1)
     (+0.25d0  0 -1)
     (+0.25d0 +1  0)
     (+0.25d0 -1  0))))

(defbenchmark stencil-five-point-2d (nbytes)
  (stencil-bench
   nbytes
   10
   '((+0.4d0  0 +1)
     (+0.4d0  0 -1)
     (-0.6d0  0  0)
     (+0.4d0 +1  0)
     (+0.4d0 -1  0))))

(defbenchmark stencil-3x3 (nbytes)
  (stencil-bench
   nbytes
   10
   '(
     (+0.1d0 +1 +1)
     (+0.1d0  0 +1)
     (+0.1d0 -1 +1)

     (+0.1d0 +1  0)
     (+0.2d0  0  0)
     (+0.1d0 -1  0)

     (+0.1d0 +1 -1)
     (+0.1d0  0 -1)
     (+0.1d0 -1 -1))))

(defbenchmark stencil-nine-point-2d (nbytes)
  (stencil-bench
   nbytes
   10
   (let ((a (coerce 1/24 'double-float))
         (b (coerce 2/24 'double-float))
         (c (coerce 1/2 'double-float)))
     (assert (= 1 (+ (* 4 a) (* 4 b) c)))
     `((,c  0  0)
       (,a  0 +2)
       (,a  0 -2)
       (,a +2  0)
       (,a -2  0)
       (,b  0 +1)
       (,b  0 -1)
       (,b +1  0)
       (,b -1  0)))))

(defbenchmark stencil-avg-2d (nbytes)
  (stencil-bench
   nbytes
   10
   '((0.25d0  0 +1)
     (0.25d0  0 -1)
     (0.25d0 +1  0)
     (0.25d0 -1  0))))

(defbenchmark stencil-avg-3d (nbytes)
  (petalisp.core:with-inspection (:graph))
  (stencil-bench
   nbytes
   10
   `((,(/ 1 6d0)  0  0 +1)
     (,(/ 1 6d0)  0  0 -1)
     (,(/ 1 6d0)  0 +1  0)
     (,(/ 1 6d0)  0 -1  0)
     (,(/ 1 6d0) +1  0  0)
     (,(/ 1 6d0) -1  0  0))))
