(in-package :common-lisp-user)

(defpackage :petalisp-examples-linear-algebra
  (:shadowing-import-from :petalisp :set-difference)
  (:use :cl :alexandria :petalisp :named-readtables)
  (:export
   #:matrix
   #:square-matrix
   #:zeros
   #:eye
   #:transpose
   #:norm
   #:dot
   #:asum
   #:argmax
   #:matmul
   #:lu))

(in-package :petalisp-examples-linear-algebra)

(in-readtable petalisp-readtable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Matrix Utilities

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
                                   :input-mask (vector i)
                                   :output-rank 0)))
    ((shape (range i) (range j)) (reshape x (make-transformation
                                             :input-mask (vector i j)
                                             :output-rank 0)))))

(trivia:defpattern matrix (m n)
  (with-gensyms (it)
    `(trivia:guard1 ,it (strided-array-p ,it)
                    (shape ,it) (shape (range 1 ,m) (range 1 ,n)))))

(trivia:defpattern square-matrix (m)
  (with-gensyms (g)
    `(matrix (and ,m ,g) (= ,g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Linear Algebra Subroutines

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
  (coerce-to-scalar
   (matmul
    (transpose x)
    (coerce-to-matrix y))))

(defun norm (x)
  (α #'sqrt (dot x x)))

(defun asum (x)
  (coerce-to-scalar
   (β #'+ (α #'abs (coerce-to-matrix x)))))

(defun argmax (x)
  (β (lambda (li lv ri rv)
       (if (>= (abs lv) (abs rv))
           (values li lv)
           (values ri rv)))
     (indices x) x))

(defun matmul (A B)
  (β #'+
     (α #'*
        (reshape (coerce-to-matrix A) (τ (m n) (n m 1)))
        (reshape (coerce-to-matrix B) (τ (n k) (n 1 k))))))

(defun pivot-and-value (A d)
  (setf A (coerce-to-matrix A))
  (trivia:ematch A
    ((square-matrix m)
     (assert (<= 1 d m))
     (multiple-value-bind (p v)
         (argmax (reshape A (make-shape (range d m) (range d))))
       (let ((p (coerce-to-scalar p))
             (v (coerce-to-scalar v)))
         ;(schedule A p v)
         (compute p v))))))

(defun swap-rows (A i j)
  (setf A (coerce-to-matrix A))
  (trivia:ematch A
    ((matrix m n)
     (assert (<= 1 i m))
     (assert (<= 1 j m))
     (if (= i j)
         A
         (let ((si (make-shape (range i) (range 1 n)))
               (sj (make-shape (range j) (range 1 n))))
           (fuse* A
                  (reshape A sj si)
                  (reshape A si sj)))))))

(defun lu (A)
  (setf A (coerce-to-matrix A))
  (trivia:ematch A
    ((square-matrix m)
     (labels
         ((rec (d P L U)
            (if (= d m)
                (compute (transpose P) L U)
                (multiple-value-bind (pivot value)
                    (pivot-and-value U d)
                  (assert (not (zerop value)))
                  (let* ((P (swap-rows P d pivot))
                         (L (swap-rows L d pivot))
                         (U (swap-rows U d pivot))
                         (S (α/ (reshape U (make-shape (range (1+ d) m) (range d)))
                                (coerce-to-matrix value))))
                    (rec (1+ d)
                         P
                         (fuse* L S (reshape 1 (make-shape (range d) (range d))))
                         (fuse* U
                                (α- (reshape U (make-shape (range (1+ d) m) (range d m)))
                                    (α* S (reshape U (make-shape (range d) (range d m))))))))))))
       (rec 1 (eye m) (zeros m) A)))))
