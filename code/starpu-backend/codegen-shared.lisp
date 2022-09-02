;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.starpu-backend)

;;; Utilities for converting Petalisp blueprints to C++, CUDA, or Lisp
;;; code.  We use the following naming convention for generated variables:
;;;
;;; startN, endN, stepN are the iteration space parameters of the Nth axis.
;;;
;;; iN is the iteration variable of the Nth loop.
;;;
;;; srcK is the data pointer of the Kth source array.
;;;
;;; dstKsN is the stride of the Nth axis of the Kth destination array.
;;;
;;; dstKoN is the offset of the Nth axis of the Kth destination array.
;;;
;;; The variables srcK, srcKsN, and srcKoN are analogously to their dst
;;; counterparts, but for source arrays.

;;; The current blueprint.
(defvar *blueprint*)

;;; The rank of the iteration space of the current blueprint.
(defvar *iteration-space-rank*)

;;; A list with one entry per iteration space axis, whose entries are
;;; either :CONTIGUOUS or :STRIDED.
(defvar *iteration-space-info*)

;;; An (TYPE RANK) info whose Kth entry describes the Kth destination array.
(defvar *dst-array-info*)

;;; An (TYPE RANK) info whose Kth entry describes the Kth source array.
(defvar *src-array-info*)

;;; An ulist of instruction blueprints.
(defvar *instruction-blueprint-ulist*)

;;; A boolean, indicating whether the generated code should print all its
;;; operations to stdout.
(defparameter *emit-verbose-code* nil)

(defmacro with-blueprint-info ((blueprint) &body body)
  `(call-with-blueprint-info ,blueprint (lambda () ,@body)))

(defun call-with-blueprint-info (blueprint thunk)
  (flet ((umap (fn ulist)
           (loop for ucons = ulist then (ucons:ucdr ucons) until (null ucons)
                 collect (funcall fn (ucons:ucar ucons))))
         (decode-array-info (info)
           (trivia:ematch info
             ((ucons:ulist ntype rank)
              (list (ntype-c-type ntype) rank)))))
    (trivia:ematch blueprint
      ((ucons:ulist* iteration-space-info dst-info src-info instruction-blueprint-ulist)
       (let* ((*blueprint* blueprint)
              (*iteration-space-info* (ucons:list-from-ulist iteration-space-info))
              (*iteration-space-rank* (length *iteration-space-info*))
              (*dst-array-info* (umap #'decode-array-info dst-info))
              (*src-array-info* (umap #'decode-array-info src-info))
              (*instruction-blueprint-ulist* instruction-blueprint-ulist))
         (funcall thunk))))))

(defun write-instruction (target-variable instruction stream)
  (trivia:match instruction
    ((ucons:ulist* :call number-of-values operator inputs)
     (unless (= 1 number-of-values)
       (error "Cannot (yet) create C++ kernels containing multiple valued functions."))
     (multiple-value-bind (type name kind)
         (decode-operator operator)
       (let ((input-numbers (mapcar #'second (ucons:tree-from-utree inputs))))
         (ecase kind
           (:infix
            (assert (= 2 (length input-numbers)))
            (format stream "    ~A ~A = v~D ~A v~D;~%"
                    type
                    target-variable
                    (first input-numbers)
                    name
                    (second input-numbers)))
           (:prefix
            (format stream "    ~A ~A = ~A(~{v~D~^, ~});~%"
                    type
                    target-variable
                    name
                    input-numbers)))
         (when *emit-verbose-code*
           (format stream "    printf(\"~A = ~A\\n\", v~D);~%"
                   target-variable
                   (type-format-string type)
                   target-variable)))))
    ((ucons:ulist* :load buffer-number irefs)
     (format stream "    ~A ~A = src~D["
             (first (nth buffer-number *src-array-info*))
             target-variable
             buffer-number)
     (write-irefs irefs (format nil "src~D" buffer-number) stream)
     (format stream "];~%")
     (when *emit-verbose-code*
       (format stream "    printf(\"~A = ~A\\n\", ~A);~%"
               target-variable
               (type-format-string (first (nth buffer-number *src-array-info*)))
               target-variable)))
    ((ucons:ulist* :store input buffer-number irefs)
     (format stream "    dst~D[" buffer-number)
     (write-irefs irefs (format nil "dst~D" buffer-number) stream)
     (format stream "] = v~D;~%" (ucons:ucar (ucons:ucdr input))))
    ((ucons:ulist :iref iref)
     (format stream "    int64_t ~A = " target-variable)
     (write-iref iref stream)
     (format stream ";~%"))))

(defun write-irefs (irefs prefix stream)
  (loop for (iref rest) on (ucons:list-from-ulist irefs) for axis from 0 do
    (format stream "(")
    (write-iref iref stream)
    (if (null rest)
        (format stream " - ~Ao~D)"
                prefix axis)
        (format stream " * ~As~d - ~Ao~D) + "
                prefix axis prefix axis)))
  (when (null irefs) (format stream "0")))

(defun write-iref (iref stream)
  (trivia:ematch iref
    ((ucons:ulist permutation scaling offset)
     (cond ((= scaling 0)
            (format stream "~D" offset))
           ((and (= offset 0) (= scaling 1))
            (format stream "i~D" permutation))
           ((= scaling 1)
            (format stream "(i~D + ~D)" permutation offset))
           ((= offset 0)
            (format stream "(i~D * ~D)" permutation scaling))
           (t
            (format stream "(i~D * ~D + ~D)" permutation scaling offset))))))

(defun write-prologue (name stream)
  (when *emit-verbose-code*
    (format stream "  printf(\"Start executing ~A\\n\");~%" name))
  ;; Declare the iteration variables.
  (loop for axis below *iteration-space-rank* do
    (format stream "  int64_t start~D, end~D, step~D;~%" axis axis axis))
  ;; Declare the variables for each array data pointer and the
  ;; corresponding offsets and strides.
  (loop for (type rank) in *dst-array-info* for axis from 0 do
    (format stream "  ~A* __restrict dst~D;~@[ uint64_t ~{dst~Do~D~^, ~};~]~@[ uint64_t ~{dst~Ds~D~^, ~};~]~%"
            type axis
            (loop for index from 0 below rank
                  collect axis
                  collect index)
            (loop for index from 0 below (1- rank)
                  collect axis
                  collect index)))
  (loop for (type rank) in *src-array-info* for axis from 0 do
    (format stream "  ~A* __restrict src~D;~@[ uint64_t ~{src~Do~D~^, ~};~]~@[ uint64_t ~{src~Ds~D~^, ~};~]~%"
            type axis
            (loop for index from 0 below rank
                  collect axis
                  collect index)
            (loop for index from 0 below (1- rank)
                  collect axis
                  collect index)))
  ;; Unpack all StarPU arguments.
  (let ((arguments
          (append
           ;; Unpack the iteration space bounds.
           (loop for axis below *iteration-space-rank*
                 collect (format nil "start~D" axis)
                 collect (format nil "end~D" axis)
                 collect (format nil "step~D" axis))
           ;; Unpack the buffer offsets.
           ;; Unpack strides that couldn't be encoded as buffer dimensions.
           (loop for (nil rank) in *dst-array-info* for index from 0
                 append
                 (append
                  (loop for axis from 0 below rank
                        collect (format nil "dst~Do~D" index axis))
                  (loop for axis from 3 below (1- rank)
                        collect (format nil "dst~Ds~D" index axis))))
           (loop for (nil rank) in *src-array-info* for index from 0
                 append
                 (append
                  (loop for axis from 0 below rank
                        collect (format nil "src~Do~D" index axis))
                  (loop for axis from 3 below (1- rank)
                        collect (format nil "src~Ds~D" index axis)))))))
    (format stream "  starpu_codelet_unpack_args(args, ~{&~A~^, ~});~%"
            arguments)
    (when *emit-verbose-code*
      (loop for argument in arguments do
        (format stream "  printf(\"~A = %lli\\n\", ~:*~A);~%"
                argument))))
  ;; Unpack all StarPU buffers.
  (let ((buffer-number -1))
    (loop for (type rank) in *dst-array-info* for index from 0 do
      (incf buffer-number)
      (case rank
        ((0 1)
         (format stream "  dst~D = (~A*)STARPU_VECTOR_GET_PTR(buffers[~D]);~%"
                 index type buffer-number))
        (2
         (format stream "  dst~D = (~A*)STARPU_MATRIX_GET_PTR(buffers[~D]);~%"
                 index type buffer-number)
         (format stream "  dst~Ds0 = STARPU_MATRIX_GET_NY(buffers[~D]);~%"
                 index buffer-number))
        (otherwise
         (format stream "  dst~D = (~A*)STARPU_BLOCK_GET_PTR(buffers[~D]);~%"
                 index type buffer-number)
         (format stream "  dst~Ds1 = STARPU_BLOCK_GET_NZ(buffers[~D])~@[ * dst~Ds2~];~%"
                 index buffer-number (and (> rank 3) index))
         (format stream "  dst~Ds0 = STARPU_BLOCK_GET_NY(buffers[~D]) * dst~Ds1;~%"
                 index buffer-number index))))
    (loop for (type rank) in *src-array-info* for index from 0 do
      (incf buffer-number)
      (case rank
        ((0 1)
         (format stream "  src~D = (~A*)STARPU_VECTOR_GET_PTR(buffers[~D]);~%"
                 index type buffer-number))
        (2
         (format stream "  src~D = (~A*)STARPU_MATRIX_GET_PTR(buffers[~D]);~%"
                 index type buffer-number)
         (format stream "  src~Ds0 = STARPU_MATRIX_GET_NY(buffers[~D]);~%"
                 index buffer-number))
        (otherwise
         (format stream "  src~D = (~A*)STARPU_BLOCK_GET_PTR(buffers[~D]);~%"
                 index type buffer-number)
         (format stream "  src~Ds1 = STARPU_BLOCK_GET_NZ(buffers[~D])~@[ * src~Ds2~];~%"
                 index buffer-number (and (> rank 3) index))
         (format stream "  src~Ds0 = STARPU_BLOCK_GET_NY(buffers[~D]) * src~Ds1;~%"
                 index buffer-number index))))))

(defun write-epilogue (name stream)
  (when *emit-verbose-code*
    (format stream "  printf(\"Done executing ~A\\n\");~%" name)
    (format stream "  fflush(stdout);~%")))

(defun ntype-c-type (ntype)
  (petalisp.type-inference:ntype-subtypecase ntype
    (short-float "float")
    (single-float "float")
    (double-float "double")
    (long-float "double")
    ((unsigned-byte 8) "uint8_t")
    ((unsigned-byte 16) "uint16_t")
    ((unsigned-byte 32) "uint32_t")
    ((unsigned-byte 64) "uint64_t")
    ((signed-byte 8) "int8_t")
    ((signed-byte 16) "int16_t")
    ((signed-byte 32) "int32_t")
    ((signed-byte 64) "int64_t")
    (t (error "Cannot create C++ kernels operating on values of type ~S."
              (petalisp.type-inference:type-specifier ntype)))))

(defun type-format-string (type)
  (trivia:ematch type
    ("float" "%g")
    ("double" "%g")))

(defparameter *operator-table*
  (alexandria:alist-hash-table
   '(;; coerce
     (petalisp.type-inference::coerce-to-short-float "float" "(float)" :prefix)
     (petalisp.type-inference::coerce-to-single-float "float" "(float)" :prefix)
     (petalisp.type-inference::coerce-to-double-float "double" "(double)" :prefix)
     (petalisp.type-inference::coerce-to-long-float "double" "(double)" :prefix)
     ;; short-float
     (petalisp.type-inference::short-float+ "float" "+" :infix)
     (petalisp.type-inference::short-float- "float" "-" :infix)
     (petalisp.type-inference::short-float* "float" "*" :infix)
     (petalisp.type-inference::short-float/ "float" "/" :infix)
     (petalisp.type-inference::short-float-abs "float" "fabs" :prefix)
     (petalisp.type-inference::short-float-from-short-float "float" "(float)" :prefix)
     (petalisp.type-inference::short-float-from-double-float "float" "(float)" :prefix)
     (petalisp.type-inference::short-float-from-long-float "float" "(float)" :prefix)
     (petalisp.type-inference::short-float-cos "float" "cos" :prefix)
     (petalisp.type-inference::short-float-exp "float" "exp" :prefix)
     (petalisp.type-inference::short-float-ln "float" "ln" :prefix)
     (petalisp.type-inference::short-float-max "float" "fmax" :prefix)
     (petalisp.type-inference::short-float-min "float" "fmin" :prefix)
     (petalisp.type-inference::short-float-sin "float" "sin" :prefix)
     (petalisp.type-inference::short-float-sqrt "float" "sqrt" :prefix)
     (petalisp.type-inference::short-float-tan "float" "tan" :prefix)
     (petalisp.type-inference::short-float-unary- "float" "-" :prefix)
     ;; single-float
     (petalisp.type-inference::single-float+ "float" "+" :infix)
     (petalisp.type-inference::single-float- "float" "-" :infix)
     (petalisp.type-inference::single-float* "float" "*" :infix)
     (petalisp.type-inference::single-float/ "float" "/" :infix)
     (petalisp.type-inference::single-float-abs "float" "fabs" :prefix)
     (petalisp.type-inference::single-float-from-short-float "float" "(float)" :prefix)
     (petalisp.type-inference::single-float-from-double-float "float" "(float)" :prefix)
     (petalisp.type-inference::single-float-from-long-float "float" "(float)" :prefix)
     (petalisp.type-inference::single-float-cos "float" "cos" :prefix)
     (petalisp.type-inference::single-float-exp "float" "exp" :prefix)
     (petalisp.type-inference::single-float-ln "float" "ln" :prefix)
     (petalisp.type-inference::single-float-max "float" "fmax" :prefix)
     (petalisp.type-inference::single-float-min "float" "fmin" :prefix)
     (petalisp.type-inference::single-float-sin "float" "sin" :prefix)
     (petalisp.type-inference::single-float-sqrt "float" "sqrt" :prefix)
     (petalisp.type-inference::single-float-tan "float" "tan" :prefix)
     (petalisp.type-inference::single-float-unary- "float" "-" :prefix)
     ;; double-float
     (petalisp.type-inference::double-float+ "double" "+" :infix)
     (petalisp.type-inference::double-float- "double" "-" :infix)
     (petalisp.type-inference::double-float* "double" "*" :infix)
     (petalisp.type-inference::double-float/ "double" "/" :infix)
     (petalisp.type-inference::double-float-abs "double" "fabs" :prefix)
     (petalisp.type-inference::double-float-from-short-float "double" "(double)" :prefix)
     (petalisp.type-inference::double-float-from-single-float "double" "(double)" :prefix)
     (petalisp.type-inference::double-float-from-long-float "double" "(double)" :prefix)
     (petalisp.type-inference::double-float-cos "double" "cos" :prefix)
     (petalisp.type-inference::double-float-exp "double" "exp" :prefix)
     (petalisp.type-inference::double-float-ln "double" "ln" :prefix)
     (petalisp.type-inference::double-float-max "double" "fmax" :prefix)
     (petalisp.type-inference::double-float-min "double" "fmin" :prefix)
     (petalisp.type-inference::double-float-sin "double" "sin" :prefix)
     (petalisp.type-inference::double-float-sqrt "double" "sqrt" :prefix)
     (petalisp.type-inference::double-float-tan "double" "tan" :prefix)
     (petalisp.type-inference::double-float-unary- "double" "-" :prefix)
     ;; long-float
     (petalisp.type-inference::long-float+ "double" "+" :infix)
     (petalisp.type-inference::long-float- "double" "-" :infix)
     (petalisp.type-inference::long-float* "double" "*" :infix)
     (petalisp.type-inference::long-float/ "double" "/" :infix)
     (petalisp.type-inference::long-float-abs "double" "fabs" :prefix)
     (petalisp.type-inference::long-float-from-short-float "double" "(double)" :prefix)
     (petalisp.type-inference::long-float-from-single-float "double" "(double)" :prefix)
     (petalisp.type-inference::long-float-from-long-float "double" "(double)" :prefix)
     (petalisp.type-inference::long-float-cos "double" "cos" :prefix)
     (petalisp.type-inference::long-float-exp "double" "exp" :prefix)
     (petalisp.type-inference::long-float-ln "double" "ln" :prefix)
     (petalisp.type-inference::long-float-max "double" "fmax" :prefix)
     (petalisp.type-inference::long-float-min "double" "fmin" :prefix)
     (petalisp.type-inference::long-float-sin "double" "sin" :prefix)
     (petalisp.type-inference::long-float-sqrt "double" "sqrt" :prefix)
     (petalisp.type-inference::long-float-tan "double" "tan" :prefix)
     (petalisp.type-inference::long-float-unary- "double" "-" :prefix))))

(defun decode-operator (operator)
  (values-list
   (or (gethash operator *operator-table*)
       (error "Cannot create C++ kernels containing ~S operators." operator))))
