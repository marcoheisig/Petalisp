;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defun α (operator object &rest more-objects)
  (let* ((objects
           (mapcar #'lisp->petalisp (list* object more-objects)))
         (index-space
           (reduce #'broadcast objects))
         (objects
           (mapcar
            (lambda (object)
              (repetition object index-space))
            objects)))
    (apply #'application operator objects)))

(defun β (operator object)
  (reduction operator object))

(defun fuse (object &rest more-objects)
  (let* ((objects (list* object more-objects))
         (current ())
         (new ()))
    (dolist (b objects)
      (let ((b-intersects nil))
        (dolist (a current)
          (let ((a∩b (intersection a b)))
            (cond
              ((not a∩b) (push a new))
              (t
               (setf b-intersects t)
               (push (reference b a∩b TODO) new)
               (dolist (difference (difference a b))
                 (push (reference a difference TODO) new))
               (dolist (difference (difference b a))
                 (push (reference b difference TODO) new))))))
        (unless b-intersects (push b new)))
      (psetf current new new ()))
    (apply #'fusion current)))

(defun repeat (object space)
  (repetition object space))

(defun -> (object &rest subspaces-and-transformations)
  (let ((target-space (index-space object))
        (transformation (identity-transformation (dimension object))))
    (dolist (x subspaces-and-transformations)
      (etypecase x
        (transformation
         (zapf target-space (transform % x))
         (zapf transformation (compose x %)))
        (index-space
         (assert (subspace? x target-space))
         (setf target-space x))))
    (let ((source-space (transform target-space (invert transformation))))
      (reference object source-space transformation))))

(defmacro subspace (space &rest dimensions)
  (with-gensyms (dim)
    (once-only (space)
      `(symbol-macrolet
           ((,(intern "START") (range-start (aref (ranges ,space) ,dim)))
            (,(intern "STEP") (range-step (aref (ranges ,space) ,dim)))
            (,(intern "END") (range-end (aref (ranges ,space) ,dim))))
         (make-index-space
          ,@(loop for form in dimensions and d from 0
                  collect `(let ((,dim ,d)) (range ,@form))))))))

(defmacro σ (&rest ranges)
  `(make-index-space
    ,@(loop for range in ranges
            collect `(range ,@range))))

(defmacro τ (symbols mappings)
  `(classify-transformation
    (lambda ,symbols
      (declare (ignorable ,@symbols))
      (values ,@mappings))
    ,(length symbols)
    ,(length mappings)))

(defun classify-affine-transformation (f input-dimension output-dimension)
  (let* ((zeroes (make-list input-dimension :initial-element 0))
         (ones (make-list input-dimension :initial-element 1))
         (twos (make-list input-dimension :initial-element 2))
         (permuted-translation
           (multiple-value-list
            (apply f zeroes)))
         (args zeroes) ; shamelessly reusing memory here
         (permuted-scaling
           (mapcar #'-
                   (multiple-value-list (apply f ones))
                   permuted-translation))
         (another-scaling
           (mapcar (lambda (a b) (/ (- a b) 2))
                   (multiple-value-list (apply f twos))
                   permuted-translation))
         (permutation (make-array output-dimension :initial-element -1))
         (affine-coefficients (make-array `(,output-dimension 2))))
    (unless (every #'= permuted-scaling another-scaling)
      (error "The transformation is not linear."))
    ;; determine the permutation
    (flet ((outpos (inpos) ; the output corresponding to the nth input
             (setf (nth inpos args) 1)
             (let ((occurences
                     (loop for i below output-dimension
                           and a in (multiple-value-list (apply f args))
                           and b in permuted-translation
                           when (/= a b) collect i)))
               (setf (nth inpos args) 0)
               (cond
                 ((null occurences) nil)
                 ((= 1 (length occurences)) (car occurences))
                 (t (error "Input ~d affects more than one output." inpos))))))
      (loop for inpos below input-dimension do
        (let ((outpos (outpos inpos)))
          (when outpos
            (unless (= -1 (aref permutation outpos))
              (error "Output argument ~d depends on more than one variable." outpos))
            (setf (aref permutation outpos) inpos)))))
    ;; determine the affine coefficients
    (loop for outpos below output-dimension do
      (setf (aref affine-coefficients outpos 0)
            (nth outpos permuted-scaling))
      (setf (aref affine-coefficients outpos 1)
            (nth outpos permuted-translation)))
    (make-instance
     'affine-transformation
     :affine-coefficients affine-coefficients
     :permutation permutation
     :input-dimension input-dimension
     :output-dimension output-dimension)))

(defun classify-transformation (f nargin nargout)
  (or
   (classify-affine-transformation f nargin nargout)
   (error "Unknown transformation.")))
