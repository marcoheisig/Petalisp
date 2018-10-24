;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(in-package :petalisp-native-backend)

;;; A form builder is a code generation utility.  A programmer can add any
;;; number of forms to a form builder.  Afterwards, one can extract a
;;; single form that is behaviorally equivalent to evaluating all the forms
;;; added so far.  Form builders are somewhat custom tailored towards the
;;; needs of Petalisp, so they treat all functions as if they were
;;; referentially transparent and they do not handle special forms.

;;; Return the form built by FORM-BUILDER.
(defgeneric form (form-builder))

;;; This is the primitive form building operator.  It appends FORM directly
;;; to the supplied FORM-BUILDER.  The optional argument TAIL describes the
;;; cons cell to whose cdr PUSH-FORM should add the next form.  It defaults
;;; to the cons cell that is implicitly created to contain FORM.
(defgeneric insert-form (form form-builder &optional tail))

;;; This is the smart form building operator.  It takes a "flat form",
;;; i.e., a form that only contains symbols or self-evaluating objects.  It
;;; then integrates another form into FORM-BUILDER that binds
;;; NUMBER-OF-VALUES variables to the values returned by FLAT-FORM and
;;; returns these variables as multiple values.
(defgeneric add-flat-form (flat-form form-builder number-of-values))

(defun add-form (form form-builder &optional (number-of-values 1))
  (labels ((pseudo-eval (form n)
             (trivia:ematch form
               ((list* function arguments)
                (add-flat-form
                 `(,function . ,(mapcar #'pseudo-eval-1 arguments))
                 form-builder n))
               (_ form)))
           (pseudo-eval-1 (form)
             (pseudo-eval form 1)))
    (pseudo-eval form number-of-values)))

(defclass form-builder ()
  ())

(defclass simple-form-builder (form-builder)
  ((%form :initarg :form :reader form)
   (%tail :initarg :tail :accessor tail)))

(defun make-simple-form-builder (form &optional (tail (last form)))
  (make-instance 'simple-form-builder
    :form form
    :tail tail))

(defmethod insert-form
    ((form cons) (simple-form-builder simple-form-builder) &optional tail)
  (let ((cons (list form)))
    (setf (cdr (tail simple-form-builder)) cons)
    (setf (tail simple-form-builder)
          (if (consp tail) tail cons))
    form))

(defmethod add-flat-form
    (form (simple-form-builder simple-form-builder) number-of-values)
  (let* ((symbols (loop repeat number-of-values collect (gensym)))
         (tail (list form)))
    (insert-form `(bind ,symbols . ,tail) simple-form-builder tail)
    (values-list symbols)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A form builder for loops

(defclass loop-form-builder (simple-form-builder)
  ())

(defun make-loop-form-builder (var type start step end)
  (let ((form `(loop for ,var ,type from ,start by ,step below ,end do)))
    (make-instance 'loop-form-builder
      :form form
      :tail (last form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A form builder for reductions

(defclass reduction-form-builder (simple-form-builder)
  ())

(defun make-reduction-form-builder (var type min max reduction-spec)
  (let* ((tail (cons nil nil)))
    (make-instance 'reduction-form-builder
      :form
      `(labels ((divide-and-conquer (min max)
                  (declare (type ,type min max))
                  (if (= min max)
                      (let ((,var min)) . ,tail)
                      (let* ((size (- max min))
                             (mid (+ min (floor size 2))))
                        (multiple-value-call ,(make-reduction-lambda reduction-spec)
                          (divide-and-conquer min mid)
                          (divide-and-conquer (1+ mid) max))))))
         (divide-and-conquer ,min ,max))
      :tail tail)))

(defun iota-map (function n)
  (loop for i below n collect (funcall function i)))

(defun make-reduction-lambda (reduction-spec)
  (let* ((k (loop for elt in reduction-spec sum (cdr elt)))
         (left-symbols (iota-map #'left-symbol k))
         (right-symbols (iota-map #'right-symbol k))
         (result-symbols (iota-map #'result-symbol k))
         (form-builder
           (make-simple-form-builder
            `(lambda ,(append left-symbols right-symbols)))))
    (loop for (operator . arity) in reduction-spec
          for start = 0 then end
          for end = arity then (+ end arity) do
            (let ((tail (list `(,operator ,@(subseq left-symbols start end)
                                          ,@(subseq right-symbols start end)))))
              (insert-form
               `(bind ,(subseq result-symbols start end) . ,tail)
               form-builder
               tail)))
    (insert-form `(values ,@result-symbols) form-builder)
    (form form-builder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A form builder for kernels

(defclass kernel-form-builder (form-builder)
  (;; An array of form builders, with one more element than there are
   ;; ranges in the iteration space.  Element zero is the form builder of
   ;; the innermost loop (or reduction) and the last element is the form
   ;; builder of the outermost form that surrounds all loops.
   (%form-builders :initarg :form-builders :reader form-builders)
   ;; A hash table mapping from symbols to the index of their defining form
   ;; builder in the vector FORM-BUILDERS.
   (%symbol-table :initarg :symbol-table :reader symbol-table)
   ;; A hash table mapping from s-expressions to their unique number.  It
   ;; is used for common subexpression elimination, to ensure that
   ;; expressions that are EQUAL are only evaluated once.
   (%instruction-table :initarg :instruction-table :reader instruction-table)))

(defun make-kernel-form-builder (ranges reduction-spec)
  (let* ((n-ranges (length ranges))
         (form-builders (make-array (1+ n-ranges)))
         (instruction-table (make-hash-table :test #'equal))
         (symbol-table (make-hash-table :test #'eq))
         (k-f-b (make-instance 'kernel-form-builder
                :form-builders form-builders
                :symbol-table symbol-table
                :instruction-table instruction-table)))
    (prog1 k-f-b
      ;; Create a form builder for the outermost context.
      (let ((index n-ranges))
        (setf (gethash 'ranges symbol-table) index)
        (setf (gethash 'arrays symbol-table) index)
        (setf (gethash 'functions symbol-table) index)
        (setf (aref form-builders index)
              (make-simple-form-builder
               `(lambda (ranges arrays functions)))))
      ;; Create one form builder for each loop or reduction, innermost-first.
      (loop for (size-bits step-bits type) in ranges
            for index from 0
            for var = (index-symbol index)
            for start = (add-form `(aref ranges ,(+ (* index 3) 0)) k-f-b)
            for step = (add-form `(aref ranges ,(+ (* index 3) 1)) k-f-b)
            for end = (add-form `(aref ranges ,(+ (* index 3) 2)) k-f-b)
            do (setf (gethash var symbol-table) index)
               (setf (aref form-builders index)
                     (if (and (zerop index)
                              (not (null reduction-spec)))
                         (make-reduction-form-builder var type 0 end reduction-spec)
                         (make-loop-form-builder var type start step end)))))))

(defmethod form ((kernel-form-builder kernel-form-builder))
  (with-accessors ((form-builders form-builders)) kernel-form-builder
    (form
     (reduce (lambda (left right)
               (insert-form (form left) right)
               right)
             form-builders))))

(defmethod add-flat-form
    ((flat-form cons)
     (kernel-form-builder kernel-form-builder)
     (number-of-values integer))
  (with-accessors ((symbol-table symbol-table)
                   (form-builders form-builders)) kernel-form-builder
    (let* ((index (loop for argument in (rest flat-form)
                        when (symbolp argument)
                          minimize (gethash argument symbol-table)))
           (form-builder (aref form-builders index))
           (symbols (multiple-value-list
                     (add-flat-form flat-form form-builder number-of-values))))
      (loop for symbol in symbols do
        (setf (gethash symbol symbol-table) index))
      (values-list symbols))))

;;; Common Subexpression Elimination.
(defmethod add-flat-form :around
    ((flat-form cons)
     (kernel-form-builder kernel-form-builder)
     (number-of-values integer))
  (with-accessors ((instruction-table instruction-table)) kernel-form-builder
    (let ((key (cons flat-form number-of-values)))
      (multiple-value-bind (variables present-p)
          (gethash key instruction-table)
        (if present-p
            (values-list variables)
            (let ((variables (multiple-value-list (call-next-method))))
              (setf (gethash key instruction-table) variables)
              (values-list variables)))))))
