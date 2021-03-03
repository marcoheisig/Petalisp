;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-inference)

(defmacro cmpx (name)
  (flet ((mkname (type)
           (intern (format nil "~:@(~S~S~)" type name) #.*package*)))
    `(progn
       (define-simple-instruction (,name ,(mkname 'short-float)) (generalized-boolean) (short-float short-float))
       (define-simple-instruction (,name ,(mkname 'single-float)) (generalized-boolean) (single-float single-float))
       (define-simple-instruction (,name ,(mkname 'double-float)) (generalized-boolean) (double-float double-float))
       (define-simple-instruction (,name ,(mkname 'long-float)) (generalized-boolean) (long-float long-float))

       (define-specializer ,name (real &rest more-reals)
         (if (null more-reals)
             (wrap
              (prog2-fn
               (the-real real)
               t))
             (labels ((cmp (a b)
                        (ntype-subtypecase
                            (numeric-contagion
                             (wrapper-ntype a)
                             (wrapper-ntype b))
                          ((not real) (abort-specialization))
                          (short-float
                           (wrap
                            (,(mkname 'short-float)
                             (coerce-to-short-float a)
                             (coerce-to-short-float b))))
                          (single-float
                           (wrap
                            (,(mkname 'single-float)
                             (coerce-to-single-float a)
                             (coerce-to-single-float b))))
                          (double-float
                           (wrap
                            (,(mkname 'double-float)
                             (coerce-to-double-float a)
                             (coerce-to-double-float b))))
                          (long-float
                           (wrap
                            (,(mkname 'long-float)
                             (coerce-to-long-float a)
                             (coerce-to-long-float b))))
                          (t
                           (wrap-default
                            (ntype 'generalized-boolean))))))
               (let ((a real)
                     (value (wrap t)))
                 (loop for b in more-reals for boolean = (cmp a b) do
                   (setf value (wrap (and-fn value boolean)))
                   (setf a b))
                 value)))))))

(cmpx <)
(cmpx >)
(cmpx <=)
(cmpx >=)
