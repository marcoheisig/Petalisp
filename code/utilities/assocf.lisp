;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.utilities)

(defmacro assocf (item place &rest keywords &environment environment)
  "Return a the first cons cell of the alist in PLACE whose car matches
ITEM.  If no such cons cell is found, a suitable one is created, pushed to
PLACE, and returned.

The KEYWORDS are the same as with CL:ASSOC."
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place environment)
    (destructuring-bind (alist) store-vars
      (alexandria:once-only (item)
        (alexandria:with-gensyms (entry)
          `(let* (,@(mapcar #'list vars vals)
                  (,alist ,reader-form)
                  (,entry (assoc ,item ,alist ,@keywords)))
             (if (consp ,entry)
                 ,entry
                 (let* ((,entry (list ,item))
                        (,alist (cons ,entry ,alist)))
                   ,writer-form
                   ,entry))))))))
