;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

(defun β (function &rest arrays)
  (multiple-value-bind (inputs input-shape)
      (broadcast-list-of-arrays arrays)
    (if (or (null input-shape)
            (zerop (shape-rank input-shape)))
        (empty-arrays (length inputs))
        (multiple-value-bind (output-shape reduction-range)
            (shrink-shape input-shape)
          (let* ((input-ntypes
                   (mapcar #'element-ntype inputs))
                 (reduction-ntypes
                   (infer-reduction-ntypes
                    function
                    input-ntypes
                    (integer-length (range-size reduction-range)))))
            (petalisp.type-inference:specialize
             function
             (append inputs inputs)
             #'element-ntype
             ;; Wrapping of constants.
             (lambda (constant)
               (declare (ignore constant))
               (petalisp.type-inference:give-up-specialization))
             ;; Wrapping of functions.
             (let ((n-invocations 0))
               (lambda (ntypes function arguments)
                 (cond
                   ;; Don't allow nested reduction functions.
                   ((plusp n-invocations)
                    (petalisp.type-inference:give-up-specialization))
                   ;; Only specialize when the input and output types of
                   ;; the reduction are the same.
                   ((loop for argument in arguments
                          for argument-ntype
                            in (append reduction-ntypes reduction-ntypes)
                              thereis
                              (not
                               (petalisp.type-inference:ntype=
                                (element-ntype argument)
                                argument-ntype)))
                    (petalisp.type-inference:give-up-specialization))
                   (t
                    (incf n-invocations)
                    (values-list
                     (loop for reduction-ntype in reduction-ntypes
                           for ntype = (or (pop ntypes)
                                           (petalisp.type-inference:ntype 'null))
                           for value-n from 0
                           collect
                           (make-instance 'reduction
                             :value-n value-n
                             :operator function
                             :ntype (petalisp.type-inference:ntype-union ntype reduction-ntype)
                             :shape output-shape
                             :inputs inputs)))))))
             ;; The default value returned by the specialization.
             (lambda ()
               (values-list
                (loop for ntype in reduction-ntypes
                      for value-n from 0
                      collect
                      (make-instance 'reduction
                        :value-n value-n
                        :operator function
                        :ntype ntype
                        :shape output-shape
                        :inputs inputs))))))))))

;;; Type inference of reduction ntypes starts with a function F and a list
;;; of ntypes (A1 ... Ak).  In a first iteration we infer the types of F
;;; applied to (A1 ... Ak A1 ... Ak) and obtain the types (B1 ... Bk).  In
;;; the second iteration, we have to distinguish three kinds of argument
;;; types: (A1 ... Ak B1 ... Bk), (A1 ... Ak B1 ... Bk) and (B1 ... Bk B1
;;; ... Bk).  Consequently, we also obtain three different result types (C1
;;; ... Ck), (D1 ... Dk) and (E1 ... Ek).  For the third iteration, we
;;; already have to check quite a number of cases.  The trick, however, is
;;; that many of these lists of argument types are identical.  This means
;;; we can employ dynamic programming to drastically reduce the number of
;;; actual checks.

(defun infer-reduction-ntypes (function ntypes maxdepth)
  (let ((k (length ntypes))
        (cache '()))
    (labels ((process (ntypes depth)
               (assert (notany #'consp ntypes))
               (unless (or (find ntypes cache :test #'ntypes=)
                           (= depth maxdepth))
                 (push ntypes cache)
                 (flet ((descend-into (left right)
                          (process
                           (infer-n-ntypes k function (append left right))
                           (1+ depth))))
                   (descend-into ntypes ntypes)
                   (loop for cached-ntypes in (rest cache) do
                     (descend-into ntypes cached-ntypes)
                     (descend-into cached-ntypes ntypes))))))
      (process ntypes 0))
    (apply #'mapcar #'petalisp.type-inference:ntype-union cache)))

(defun ntypes= (list-1 list-2)
  (declare (list list-1 list-2))
  (every #'petalisp.type-inference:ntype= list-1 list-2))

(defun infer-n-ntypes (n function argument-ntypes)
  (let* ((result-ntypes
           (multiple-value-list
            (petalisp.type-inference:infer-ntypes
             function
             argument-ntypes
             (lambda ()
               (values-list
                (make-list n :initial-element (petalisp.type-inference:ntype 't)))))))
         (new-n (length result-ntypes)))
    (cond ((< new-n n)
           (append
            result-ntypes
            (make-list (- n new-n) :initial-element (petalisp.type-inference:ntype 'null))))
          ((> new-n n)
           (subseq result-ntypes 0 n))
          ((= new-n n)
           result-ntypes))))
