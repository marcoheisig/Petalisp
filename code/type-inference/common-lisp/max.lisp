;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(define-simple-instruction (max short-float-max) (short-float) (short-float short-float))
(define-simple-instruction (max single-float-max) (single-float) (single-float single-float))
(define-simple-instruction (max double-float-max) (double-float) (double-float double-float))
(define-simple-instruction (max long-float-max) (long-float) (long-float long-float))

(define-specializer max (real &rest more-reals)
  (cond ((null more-reals)
         (wrap (the-real real)))
        (t
         (reduce
          (lambda (a b)
            (let ((ntype-of-a (wrapper-ntype a))
                  (ntype-of-b (wrapper-ntype b)))
              (ntype-subtypecase ntype-of-a
                ((not real) (abort-specialization))
                (short-float
                 (ntype-subtypecase ntype-of-b
                   ((not real) (abort-specialization))
                   (short-float (wrap (short-float-max a b)))
                   (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                (single-float
                 (ntype-subtypecase ntype-of-b
                   ((not real) (abort-specialization))
                   (single-float (wrap (single-float-max a b)))
                   (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                (double-float
                 (ntype-subtypecase ntype-of-b
                   ((not real) (abort-specialization))
                   (double-float (wrap (double-float-max a b)))
                   (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                (long-float
                 (ntype-subtypecase ntype-of-b
                   ((not real) (abort-specialization))
                   (long-float (wrap (long-float-max a b)))
                   (t (wrap-default (ntype-union ntype-of-a ntype-of-b)))))
                (t
                 (ntype-subtypecase ntype-of-b
                   ((not real) (abort-specialization))
                   (t (wrap-default (ntype-union ntype-of-a ntype-of-b))))))))
          more-reals
          :initial-value real))))

(defun argmax (real &rest more-reals)
  (labels ((argmax-aux (max max-index index reals)
             (declare (real max) ((and fixnum unsigned-byte) max-index index))
             (cond ((null reals)
                    max-index)
                   ((>= max (first reals))
                    (argmax-aux max max-index (1+ index) (rest reals)))
                   (t
                    (argmax-aux (first reals) (1+ index) (1+ index) (rest reals))))))
    (check-type real real)
    (argmax-aux real 0 0 more-reals)))

(define-specializer argmax (real &rest more-reals)
  (wrap-default (ntype 'argument-index)))

(define-differentiator max (real &rest more-reals) index
  (funcall (specializer 'if)
           (funcall (specializer '=)
                    (wrap-constant index)
                    (apply (specializer 'argmax) real more-reals))
           (wrap-constant 1)
           (wrap-constant 0)))
