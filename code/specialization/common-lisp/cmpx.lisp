;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(defmacro cmpx (base-name name)
  (flet ((mkname (type)
           (intern (format nil "~:@(~S.~S~)" name type) #.*package*)))
    `(progn
       (defop (,base-name ,(mkname 'integer)) (generalized-boolean) (integer integer))
       (defop (,base-name ,(mkname 'rational)) (generalized-boolean) (rational rational))
       (defop (,base-name ,(mkname 'short-float)) (generalized-boolean) (short-float short-float))
       (defop (,base-name ,(mkname 'single-float)) (generalized-boolean) (single-float single-float))
       (defop (,base-name ,(mkname 'double-float)) (generalized-boolean) (double-float double-float))
       (defop (,base-name ,(mkname 'long-float)) (generalized-boolean) (long-float long-float))
       (defop (,base-name ,name) (generalized-boolean) (real real) (a b)
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
              (coerce-to-long-float b)))))))))

(cmpx < cmplt)
(cmpx > cmpgt)
(cmpx <= cmple)
(cmpx >= cmpge)
