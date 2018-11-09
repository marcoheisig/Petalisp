(in-package :common-lisp-user)

(defpackage :petalisp-examples-linear-algebra
  (:shadowing-import-from :petalisp :set-difference)
  (:use :cl :petalisp :named-readtables)
  (:export
   #:zeros
   #:eye
   #:transpose
   #:norm
   #:dot
   #:asum
   #:amax
   #:matmul))

(in-package :petalisp-examples-linear-algebra)

(in-readtable petalisp-readtable)

(defun coerce-to-matrix (x)
  (setf x (coerce-to-strided-array x))
  (trivia:ematch (shape x)
    ;; Rank 0
    ((shape) (reshape x (τ () (1 1))))
    ;; Rank 1
    ((shape (range 1 _)) (reshape x (τ (i) (i 1))))
    ((shape (range 0 _)) (reshape x (τ (i) ((1+ i) 1))))
    ;; Rank 2
    ((shape (range 1 _) (range 1 _)) (reshape x (τ (i j) (i j))))
    ((shape (range 0 _) (range 1 _)) (reshape x (τ (i j) ((1+ i) j))))
    ((shape (range 1 _) (range 0 _)) (reshape x (τ (i j) (i (1+ j)))))
    ((shape (range 0 _) (range 0 _)) (reshape x (τ (i j) ((1+ i) (1+ j)))))))

(defun coerce-to-scalar (x)
  (setf x (coerce-to-strided-array x))
  (trivia:ematch (shape x)
    ((shape) x)
    ((shape (range i)) (reshape x (make-transformation
                                   :input-constraints (vector i)
                                   :output-rank 0)))
    ((shape (range i) (range j)) (reshape x (make-transformation
                                             :input-constraints (vector i j)
                                             :output-rank 0)))))

(defun zeros (m &optional (n m))
  (assert (plusp m))
  (assert (plusp n))
  (reshape 0 (make-shape (range 1 m) (range 1 n))))

(declaim (inline δ))
(defun δ (i j)
  (declare (type integer i j))
  (if (= i j) 1 0))

(defun eye (m &optional (n m))
  (assert (plusp m))
  (assert (plusp n))
  (let ((shape (make-shape (range 1 m) (range 1 n))))
    (αδ (indices shape 0) (indices shape 1))))

(defun transpose (x)
  (reshape
   (coerce-to-matrix x)
   (τ (m n) (n m))))

(defun dot (x y)
  (reshape
   (matmul
    (transpose x)
    (coerce-to-matrix y))
   (τ (0 0) ())))

(defun norm (x)
  (α #'sqrt (dot x x)))

(defun asum (x)
  (coerce-to-scalar
   (β #'+ (α #'abs (coerce-to-matrix x)))))

(defun amax (x)
  (flet ((amax-fn (lmax lind rmax rind)
           (if (>= (abs lmax) (abs rmax))
               (values lmax lind)
               (values rmax rind))))
    (let ((vector (coerce-to-matrix x)))
      (multiple-value-bind (max index)
          (β #'amax-fn vector (indices vector))
        (values (coerce-to-scalar max)
                (coerce-to-scalar index))))))

(defun matmul (A B)
  (β #'+
     (α #'*
        (reshape (coerce-to-matrix A) (τ (m n) (n m 1)))
        (reshape (coerce-to-matrix B) (τ (n k) (n 1 k))))))

#+nil
(defun lu (A)
  (setf A (coerce-to-matrix A))
  (trivia:ematch (shape A)
    ((shape (range 1 m) (range 1 (= m)))
     (labels ((rec (i P L U)
                (multiple-value-bind (pivot pivot-index)
                    (amax (reshape U)))))
       (rec 1 (eye m) (zeros m) A)))))
