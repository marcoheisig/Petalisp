;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

(defun numeric-contagion (type-code-1 type-code-2)
  (with-type-code-caching (type-code-1 type-code-2)
    (with-type-inference-barrier
      (check-type-code type-code-1 number)
      (check-type-code type-code-2 number)
      (flet ((parse-type-code (type-code)
               (cond
                 ((= type-code +short-float-type-code+) (values nil 0))
                 ((= type-code +single-float-type-code+) (values nil 1))
                 ((= type-code +double-float-type-code+) (values nil 2))
                 ((= type-code +long-float-type-code+) (values nil 3))
                 ((= type-code +complex-short-float-type-code+) (values t 0))
                 ((= type-code +complex-single-float-type-code+) (values t 1))
                 ((= type-code +complex-double-float-type-code+) (values t 2))
                 ((= type-code +complex-long-float-type-code+) (values t 3))
                 (t (give-up-type-inference))))
             (unparse-type-code (complexp index)
               (if complexp
                   (ecase index
                     (0 +complex-short-float-type-code+)
                     (1 +complex-single-float-type-code+)
                     (2 +complex-double-float-type-code+)
                     (3 +complex-long-float-type-code+))
                   (ecase index
                     (0 +short-float-type-code+)
                     (1 +single-float-type-code+)
                     (2 +double-float-type-code+)
                     (3 +long-float-type-code+)))))
        (multiple-value-bind (complexp-1 index-1)
            (parse-type-code type-code-1)
          (multiple-value-bind (complexp-2 index-2)
              (parse-type-code type-code-2)
            (unparse-type-code
             (or complexp-1 complexp-2)
             (max index-1 index-2))))))))

(define-type-inference-rule + (&rest type-codes)
  (if (null type-codes)
      (type-code-of 0)
      (reduce #'numeric-contagion type-codes)))

 (define-type-inference-rule cl:float-sign (type-code-1 &optional (type-code-2 type-code-1))
   (if (and (type-code-floatp type-code-1)
            (type-code-floatp type-code-2))
       type-code-2
       +empty-type-code+))
