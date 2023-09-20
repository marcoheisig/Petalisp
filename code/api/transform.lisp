;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printing

(defparameter *alphabet*
  #(:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z))

(defun transformation-input-symbol (position)
  (if (array-in-bounds-p *alphabet* position)
      (svref *alphabet* position)
      (alexandria:format-symbol :keyword "I~D" position)))

(defmethod print-object ((transformation transformation) stream)
  (if *print-readably*
      (call-next-method)
      (let* ((inputs
               (loop for input-index from 0
                     for input-constraint across (transformation-input-mask transformation)
                     if (not input-constraint)
                       collect (transformation-input-symbol input-index)
                     else
                       collect input-constraint))
             (outputs (transform-index inputs transformation)))
        (princ `(transform ,@inputs to ,@outputs) stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The TRANSFORM Notation

(defmacro transform (&rest inputs-to-outputs)
  (expand-transform inputs-to-outputs))

(defun expand-transform (forms)
  (let* ((position (position 'to forms))
         (inputs (subseq forms 0 position))
         (outputs (subseq forms (1+ position))))
    (multiple-value-bind (input-variables input-constraints)
        (parse-transform-inputs inputs)
      (multiple-value-bind (output-mask output-variables output-forms)
          (parse-transform-outputs outputs input-variables)
        `(make-transform-transformation
          ,(compute-transform-input-mask input-constraints)
          ,output-mask
          ,(compute-transform-output-functions output-variables output-forms)
          ,(apply #'vector output-variables)
          ,(apply #'vector output-forms))))))

(defun compute-transform-input-mask (input-constraints)
  (if (every #'constantp input-constraints)
      `(load-time-value (vector ,@input-constraints))
      `(vector ,@input-constraints)))

(defun compute-transform-output-functions (output-variables output-forms)
  `(vector
    ,@(mapcar
       (lambda (variable form)
         `(lambda (,variable)
            (declare (ignorable ,variable))
            ,form))
       output-variables
       output-forms)))

;;; Returns two values - a list of input variables and a list of forms for
;;; computing the corresponding constraints.
(defun parse-transform-inputs (inputs)
  (if (null inputs)
      (values '() '())
      (values-list
       (apply #'mapcar #'list (mapcar #'canonicalize-transform-input inputs)))))

(defun canonicalize-transform-input (input)
  (trivia:match input
    ((type integer)
     (list (gensym) input))
    ((type symbol)
     (list input nil))
    ((list (type symbol) _)
     input)
    (_
     (error "~@<The expression ~S is not a valid transform input.~:@>" input))))

;;; Returns three values - a list of output mask entries, a list of output
;;; variables (or NIL, if that output doesn't depend on any input
;;; variable), and a list of output forms.
(defun parse-transform-outputs (outputs input-variables)
  (loop for output in outputs
        for (mask-entry variable form) = (parse-transform-output output input-variables)
        collect mask-entry into mask-entries
        collect variable into variables
        collect form into forms
        finally (return (values (apply #'vector mask-entries) variables forms))))

(defun parse-transform-output (output input-variables)
  (trivia:match (free-variables output input-variables)
    ((list)
     (list nil (gensym) output))
    ((list variable)
     (list (position variable input-variables) variable output))
    ((list* variables)
     (error "~@<The transform output expression ~S must only depend on a single ~
                input variable, but depends on the variables ~
                ~{~#[~;and ~S~;~S ~:;~S, ~]~}~:@>"
            output variables))))

(defun make-transform-transformation
    (input-mask output-mask functions variables forms)
  (declare (simple-vector input-mask output-mask functions variables forms))
  (assert (= (length output-mask)
             (length functions)
             (length forms)
             (length variables)))
  (let* ((output-rank (length output-mask))
         (offsets (make-array output-rank :initial-element 0))
         (scalings (make-array output-rank :initial-element 1)))
    (loop for function across functions
          for output-index from 0 do
            (let* ((y-0 (funcall function 0))
                   (y-1 (funcall function 1))
                   (y-2 (funcall function 2))
                   (b y-0)
                   (a (- y-1 y-0)))
              (unless (= (+ (* 2 a) b) y-2)
                (error "~@<The form ~S is not affine ~
                           linear~@[ in the variable ~S~].~:@>"
                       (svref forms output-index)
                       (svref variables output-index)))
              (setf (svref scalings output-index) a)
              (setf (svref offsets output-index) b)))
    (make-transformation
     :input-mask input-mask
     :output-mask output-mask
     :scalings scalings
     :offsets offsets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing Free Variables
;;;
;;; In general, determining the set of free variables requires a code
;;; walker.  But if we already know the set of variables that may appear
;;; free in an expression, we can avoid the code walker and use a
;;; combination of SYMBOL-MACROLET and MACROEXPAND-ALL.

(defvar *free-variables*)

(defun register-free-variable (free-variable)
  (pushnew free-variable *free-variables*)
  nil)

(defun free-variables (expression candidates)
  (assert (notany #'null candidates))
  (let* ((*free-variables* '())
         (candidate-names (mapcar #'symbol-name candidates))
         (expander-names (mapcar #'gensym candidate-names)))
    (trivial-macroexpand-all:macroexpand-all
     `(macrolet ,(loop for candidate in candidates
                       for expander-name in expander-names
                       collect
                       `(,expander-name () (register-free-variable ',candidate)))
        (symbol-macrolet ,(loop for candidate in candidates
                                for expander-name in expander-names
                                collect `(,candidate (,expander-name)))
          ,expression)))
    *free-variables*))
