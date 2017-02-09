;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; definitions of externally visible Petalisp symbols

(in-package :petalisp)

(defun α (function object &rest more-objects)
  "Apply FUNCTION element-wise to OBJECT and MORE-OBJECTS, like a CL:MAPCAR
for Petalisp data structures. When the dimensions of some of the inputs
mismatch, the smaller objects are broadcasted where possible."
  (let* ((objects
           (mapcar #'lisp->petalisp (cons object more-objects)))
         (index-space
           (reduce #'broadcast objects))
         (objects
           (mapcar
            (lambda (object)
              (repetition object index-space))
            objects)))
    (apply #'application function objects)))

(defun β (function object)
  "Reduce the last dimension of OBJECT with FUNCTION."
  (reduction function (lisp->petalisp object)))

(defun fuse (&rest objects)
  "Combine OBJECTS into a single petalisp data structure. It is an error if
some of the inputs overlap, or if there exists no suitable data structure
to represent the fusion."
  (apply #'fusion (mapcar #'lisp->petalisp objects)))

(defun fuse* (&rest objects)
  "Combine OBJECTS into a single petalisp data structure. When some OBJECTS
overlap partially, the value of the rightmost object is used."
  (let* ((objects (mapcar #'lisp->petalisp objects))
         (pieces (apply #'subdivision objects)))
    (apply
     #'fusion
     (mapcar
      (lambda (piece)
        (reference
         (find piece objects :from-end t :test #'subspace?)
         (index-space piece)
         (identity-transformation (dimension piece))))
      pieces))))

(defun -> (data-structure &rest modifiers)
  "Manipulate DATA-STRUCTURE depending on the individual MODIFIERS. The
MODIFIERS are applied from left to right, the result of the first
modification is used as the argument to the second one and so on. The result
of the last modification is returned.

When a modifier is of type INDEX-SPACE, it denotes a selection of the given
data structure. For example the modifier (σ (7 9)) would select only the
elements with the keys 7, 8 and 9 from the given argument.

When a modifier is of type TRANSFORMATION, the argument is permuted
accordingly. For example applying the transformation (τ (m n) (n m) to a
3x10 array would result in a 10x3 array."
  (labels ((apply-modification (x modifier)
             (etypecase modifier
               (data-structure
                (if (or (/= (dimension x) (dimension modifier))
                        (subspace? x modifier))
                    (repetition x modifier)
                    (reference x (intersection modifier x)
                               (identity-transformation (dimension x)))))
               (transformation
                (reference x (index-space x) modifier)))))
    (reduce #'apply-modification modifiers
            :initial-value (lisp->petalisp data-structure))))

(defalias → ->)

(defmacro σ* (space &rest dimensions)
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

(defmacro τ (input-forms &rest output-forms)
  (loop for form in input-forms
        collect (when (integerp form) form) into input-constraints
        collect (if (symbolp form) form (gensym)) into symbols
        finally
           (return
             `(classify-transformation
               (lambda ,symbols
                 (declare (ignorable ,@symbols))
                 (values ,@output-forms))
               ,(apply #'vector input-constraints)
               ,(length output-forms)))))
