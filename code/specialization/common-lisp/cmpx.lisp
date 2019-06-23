;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(defmacro cmpx (base-name cmpname)
  (flet ((mkname (type)
           (intern (format nil "~:@(~S.~S~)" cmpname type) #.*package*)))
    `(progn
       (defop (,base-name ,(mkname 'integer)) (generalized-boolean) (integer integer))
       (defop (,base-name ,(mkname 'rational)) (generalized-boolean) (rational rational))
       (defop (,base-name ,(mkname 'short-float)) (generalized-boolean) (short-float short-float))
       (defop (,base-name ,(mkname 'single-float)) (generalized-boolean) (single-float single-float))
       (defop (,base-name ,(mkname 'double-float)) (generalized-boolean) (double-float double-float))
       (defop (,base-name ,(mkname 'long-float)) (generalized-boolean) (long-float long-float))
       (defop (,base-name ,cmpname) (generalized-boolean) (real real) (a b)
         (type-code-subtypecase (numeric-contagion a b)
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
              (coerce-to-long-float b))))))
       (define-external-rewrite-rule ,base-name (number &rest more-numbers)
         (if (null more-numbers)
             (rewrite-let () (rewrite-as t))
             (multiple-value-bind (a-type-codes a-value)
                 (process-argument number)
               (multiple-value-bind (b-type-codes b-value)
                   (process-argument (pop more-numbers))
                 (multiple-value-bind (result-type-codes result-value)
                     (rewrite-let ((a (values a-type-codes a-value))
                                   (b (values b-type-codes b-value)))
                       (rewrite-as (,cmpname a b)))
                   (loop until (null more-numbers) do
                     (setf a-type-codes b-type-codes)
                     (setf a-value b-value)
                     (multiple-value-setq (result-type-codes result-value)
                       (rewrite-let ((a (values a-type-codes a-value))
                                     (b (process-argument (pop more-numbers)))
                                     (c (values result-type-codes result-value)))
                         (rewrite-as (and-fn c (,cmpname a b))))))
                   (values result-type-codes result-value)))))))))

(cmpx < cmplt)
(cmpx > cmpgt)
(cmpx <= cmple)
(cmpx >= cmpge)
