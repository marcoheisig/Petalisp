;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(defmacro cmpx (base-name cmp)
  (flet ((mkname (type)
           (intern (format nil "~:@(~S.~S~)" cmp type) #.*package*)))
    `(progn
       (define-simple-instruction (,base-name ,(mkname 'integer)) (generalized-boolean) (integer integer))
       (define-simple-instruction (,base-name ,(mkname 'rational)) (generalized-boolean) (rational rational))
       (define-simple-instruction (,base-name ,(mkname 'short-float)) (generalized-boolean) (short-float short-float))
       (define-simple-instruction (,base-name ,(mkname 'single-float)) (generalized-boolean) (single-float single-float))
       (define-simple-instruction (,base-name ,(mkname 'double-float)) (generalized-boolean) (double-float double-float))
       (define-simple-instruction (,base-name ,(mkname 'long-float)) (generalized-boolean) (long-float long-float))
       (define-instruction (,base-name ,cmp) (generalized-boolean) (a b)
         (ntype-subtypecase
             (numeric-contagion
              (wrapper-ntype a)
              (wrapper-ntype b))
           ((not real) (abort-specialization))
           (integer
            (rewrite-as
             (,(mkname 'integer)
              (the-integer a)
              (the-integer b))))
           (rational
            (rewrite-as
             (,(mkname 'integer)
              (the-rational a)
              (the-rational b))))
           (short-float
            (rewrite-as
             (,(mkname 'short-float)
              (coerce-to-short-float a)
              (coerce-to-short-float b))))
           (single-float
            (rewrite-as
             (,(mkname 'single-float)
              (coerce-to-single-float a)
              (coerce-to-single-float b))))
           (double-float
            (rewrite-as
             (,(mkname 'double-float)
              (coerce-to-double-float a)
              (coerce-to-double-float b))))
           (long-float
            (rewrite-as
             (,(mkname 'long-float)
              (coerce-to-long-float a)
              (coerce-to-long-float b))))
           (t
            (rewrite-default
             (ntype 'generalized-boolean)))))

       (define-rule ,base-name (real &rest more-reals)
         (if (null more-reals)
             (rewrite-as
              (prog2-fn
               (the-real real)
               t))
             (let ((a real)
                   (value (rewrite-as t)))
               (loop for b in more-reals do
                 (setf value (rewrite-as (and-fn value (,cmp a b))))
                 (setf a b))
               value))))))

(cmpx < cmplt)
(cmpx > cmpgt)
(cmpx <= cmple)
(cmpx >= cmpge)
