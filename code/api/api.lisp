;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defvar *backend* (petalisp.native-backend:make-native-backend)
  "The backend on which Petalisp programs are executed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Working With Shapes

(defun indices (array-or-shape &optional (axis 0))
  "Return an array of integers, where the value of each entry (i_0 ... i_N)
is i_AXIS.  If axis is not supplied, it defaults to zero."
  (let* ((shape (shape array-or-shape))
         (rank (rank shape)))
    (assert (<= 0 axis (1- rank)))
    (make-reference
     (make-range-immediate (nth axis (ranges shape)))
     shape
     (make-transformation
      :input-rank rank
      :output-mask (vector axis)))))

(defun reshape (array &rest shapes-and-transformations)
  "Return a data structure of given SHAPE, either by selecting a subset of
the elements of DATA, or by broadcasting them.

Examples:
 (reshape 0 '(10 10))          ; Create a 10x10 array of zeros
 (reshape #(1 2 3 4) '((1 2))) ; Select the two interior entries"
  (labels ((reshape-with-shape (lazy-array shape)
             (make-reference
              lazy-array
              shape
              (broadcasting-transformation shape (shape lazy-array))))
           (reshape-with-transformation (lazy-array transformation)
             (make-reference
              lazy-array
              (transform (shape lazy-array) transformation)
              (invert-transformation transformation)))
           (reshape1 (lazy-array modifier)
             (if (shapep modifier)
                 (reshape-with-shape lazy-array modifier)
                 (reshape-with-transformation lazy-array modifier))))
    (reduce #'reshape1 shapes-and-transformations :initial-value (coerce-to-lazy-array array))))

(defun fuse (&rest arrays)
  "Combine ARRAYS into a single strided array.  It is an error if some of
the supplied arrays overlap, or if there exists no suitable strided array
to represent the fusion."
  (make-fusion (mapcar #'coerce-to-lazy-array arrays)))

(defun fuse* (&rest arrays)
  "Combine ARRAYS into a single strided array.  When some of the supplied
arguments overlap partially, the value of the rightmost object is used."
  (let ((lazy-arrays (mapcar #'coerce-to-lazy-array arrays)))
    (flet ((reference-origin (piece)
             (make-reference
              (find piece lazy-arrays :from-end t :key #'shape :test #'set-subsetp)
              piece
              (identity-transformation (rank piece)))))
      (make-fusion (mapcar #'reference-origin (subdivision (mapcar #'shape lazy-arrays)))))))

;;; Return a list of disjoint shapes. Each resulting object is a proper
;;; subspace of one or more of the arguments and their fusion covers all
;;; arguments.
(defun subdivision (shapes)
  (labels ((subtract (shapes what)
             (loop for shape in shapes
                   append (shape-difference-list shape what)))
           (shatter (dust object) ; dust is a list of disjoint shapes
             (let* ((object-w/o-dust (list object))
                    (new-dust '()))
               (loop for particle in dust do
                 (setf object-w/o-dust (subtract object-w/o-dust particle))
                 (loop for shape in (shape-difference-list particle object) do
                   (push shape new-dust))
                 (let ((it (set-intersection particle object)))
                   (unless (set-emptyp it)
                     (push it new-dust))))
               (append object-w/o-dust new-dust))))
    (trivia:ematch shapes
      ((list) '())
      ((list _) shapes)
      ((list* _ _ _)
       (reduce #'shatter shapes :initial-value nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Broadcasting

(defun broadcast-shapes (&rest objects)
  (let ((list-of-ranges (mapcar (alexandria:compose #'ranges #'shape) objects))
        (broadcast-ranges '()))
    (loop
      (let ((broadcast-range nil))
        (loop for cons on list-of-ranges do
          (unless (null (car cons))
            (let ((range (pop (car cons))))
              (cond ((null broadcast-range)
                     (setf broadcast-range range))
                    ((or (size-one-range-p range)
                         (set-equal range broadcast-range))
                     (values))
                    ((size-one-range-p broadcast-range)
                     (setf broadcast-range range))
                    (t
                     (error "~@<There is no common broadcast shape for the shapes ~
                                ~{~#[~;and ~S~;~S ~:;~S, ~]~}.~:@>"
                            (mapcar #'shape objects)))))))
        (if (null broadcast-range)
            (return (apply #'make-shape (nreverse broadcast-ranges)))
            (push broadcast-range broadcast-ranges))))))

(defun broadcast-arrays (&rest arrays)
  (let* ((lazy-arrays (mapcar #'coerce-to-lazy-array arrays))
         (shape (apply #'broadcast-shapes lazy-arrays)))
    (map-into lazy-arrays
              (lambda (lazy-array) (reshape lazy-array shape))
              lazy-arrays)))


(defun broadcasting-transformation (input-shape output-shape)
  (let* ((input-ranges (ranges input-shape))
         (output-ranges (ranges output-shape))
         (input-rank (length input-ranges))
         (output-rank (length output-ranges))
         (offsets (make-array output-rank :initial-element 0))
         (scalings (make-array output-rank :initial-element 1))
         (input-mask
           (map 'simple-vector
                (lambda (range)
                  (when (size-one-range-p range)
                    (range-start range)))
                input-ranges)))
    (loop for index below (min input-rank output-rank)
          for input-range in input-ranges
          for output-range in output-ranges do
            (let ((output-size (set-size output-range))
                  (input-size (set-size input-range)))
              (cond ( ;; Select
                     (> output-size input-size)
                     (setf (svref offsets index) 0)
                     (setf (svref scalings index) 1))
                    ( ;; Move
                     (= output-size input-size)
                     (let ((scale (/ (range-step output-range)
                                     (range-step input-range))))
                       (setf (svref scalings index) scale)
                       (setf (svref offsets index)
                             (- (range-start output-range)
                                (* scale (range-start input-range))))))
                    ( ;; Broadcast
                     (= 1 output-size)
                     (setf (svref offsets index) (range-start output-range))
                     (setf (svref scalings index) 0))
                    (t (error "Cannot broadcast the range ~S to the range ~S."
                              input-range output-range)))))
    (make-transformation
     :input-rank input-rank
     :output-rank output-rank
     :offsets offsets
     :scalings scalings
     :input-mask input-mask)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parallel MAP and REDUCE

(defun α (arg-1 arg-2 &rest more-args)
  "Apply FUNCTION element-wise to OBJECT and MORE-OBJECTS, like a CL:MAPCAR
for Petalisp data structures.  When the rank of some of the inputs
mismatch, broadcast the smaller objects."
  (multiple-value-bind (n-outputs function lazy-arrays)
      (if (integerp arg-1)
          (values arg-1 (coerce arg-2 'function) (apply #'broadcast-arrays more-args))
          (values 1 (coerce arg-1 'function) (apply #'broadcast-arrays arg-2 more-args)))
    (values-list
     (loop for value-n below n-outputs
           collect
           (make-application value-n function lazy-arrays)))))

(defun β (function array &rest more-arrays)
  (make-reduction function (apply #'broadcast-arrays array more-arrays)))

(defmacro defalias (alias function)
  `(progn (setf (fdefinition ',alias) #',function)
          (setf (documentation ',alias 'function)
                (documentation ',function 'function))))

(defalias a α)

(defalias b β)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Evaluation

(defun compute (&rest arguments)
  "Return the computed values of all ARGUMENTS."
  (compute-on-backend
   (mapcar #'coerce-to-lazy-array arguments)
   *backend*))

(defun schedule (&rest arguments)
  "Instruct Petalisp to compute all given ARGUMENTS asynchronously."
  (schedule-on-backend
   (mapcar #'coerce-to-lazy-array arguments)
   *backend*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parallel Aliases

(defmacro define-parallel-aliases (name)
  (let ((αsym (alexandria:symbolicate 'α name))
        (βsym (alexandria:symbolicate 'β name)))
    `(progn
       (declaim (inline ,αsym ,βsym))
       (defun ,αsym (&rest args)
         (apply #'α #',name args))
       (defun ,βsym (&rest args)
         (apply #'β #',name args))
       (define-compiler-macro ,αsym (&rest args)
         `(α #',name ,@args))
       (define-compiler-macro ,βsym (&rest args)
         `(β #',name ,@args))
       ',name)))
