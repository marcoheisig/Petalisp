(in-package #:petalisp.api)

(defun lazy-reduce (function &rest arrays)
  (let ((lazy-arrays (broadcast arrays)))
    (dolist (function (alexandria:ensure-list function))
      (setf lazy-arrays (lazy-reduce-aux function lazy-arrays)))
    (values-list lazy-arrays)))

(defun lazy-reduce-aux (function lazy-arrays)
  (declare (typo:function-designator function)
           (list lazy-arrays))
  (when (null lazy-arrays)
    (funcall function)
    (return-from lazy-reduce-aux (values)))
  (let* ((shape (lazy-array-shape (first lazy-arrays)))
         (rank (shape-rank shape)))
    (if (zerop rank)
        (error "Cannot reduce arrays with rank zero.")
        (let* ((nargs (length lazy-arrays))
               (range (shape-range shape 0))
               (size (range-size range)))
          (if (zerop size)
              (lazy-multireshape
               (replace
                (make-list nargs)
                (multiple-value-list (funcall function)))
               (make-shape (rest (shape-ranges shape))))
              (labels
                  ((process (n k xs ys)
                     (if (oddp n)
                         (process-odd n k xs ys)
                         (process-even n k xs ys)))
                   (process-even (n k xs ys)
                     (let* ((p (/ n 2))
                            (zs (multiple-value-list
                                 (apply #'lazy-multiple-value k function
                                        (append
                                         (lazy-multireshape xs (~ 0 n 2) (~ p))
                                         (lazy-multireshape xs (~ 1 n 2) (~ p)))))))
                       (process p k zs ys)))
                   (process-odd (n k xs ys)
                     (if (not ys)
                         (if (= n 1)
                             (lazy-multireshape xs (transform 0 to))
                             (process-even
                              (1- n)
                              k
                              (lazy-multireshape xs (~ (1- n)))
                              (lazy-multireshape xs (~ (1- n) n))))
                         (process-even
                          (1+ n)
                          k
                          (mapcar (lambda (x y) (lazy-stack (list x y))) xs ys)
                          nil))))
                (process size nargs (lazy-multireshape lazy-arrays (~ size)) nil)))))))

(defun lazy-multireshape (lazy-arrays &rest modifiers)
  (declare (list lazy-arrays))
  (mapcar
   (lambda (lazy-array)
     (apply #'lazy-reshape lazy-array modifiers))
   lazy-arrays))
