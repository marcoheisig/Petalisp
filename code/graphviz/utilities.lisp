;;;; © 2016-2021 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.graphviz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conversion of Lisp Data to Strings

(defun stringify (x)
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-length* 8)
          (*print-right-margin* 60)
          (*print-pretty* t)
          (*print-escape* nil)
          (*print-readably* nil)
          (*package* (find-package '#:petalisp.api)))
      (write-to-string x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; HTML Labels
;;;
;;; Graphviz allows label specifications in a small subset of HTML. The
;;; following functions simplify the creation of labels in the S-expression
;;; based HTML notation of CL-DOT.

(defun make-html-label (&key caption properties)
  `(:html
    ()
    (:table
     ((:border "0") (:cellborder "0") (:cellspacing "0"))
     (:tr () (:td ((:colspan "2") (:align "center")) ,caption))
     ,@(mapcar #'make-html-table-row properties))))

(defun make-html-table-row (property)
  (destructuring-bind (key . value) property
    (check-type key string)
    (check-type value string)
    `(:tr ()
          (:td ((:align "left"))
               (:b () ,key))
          (:td ((:align "left"))
               ,value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Graphviz Attribute Method Combination
;;;
;;; The appearance and behavior of Graphviz graphs, edges and nodes is
;;; determined by their respective attributes. In CL-DOT, the attributes
;;; are specified using a property list.
;;;
;;; Our goal is to have inheritance on each attribute, e.g. to provide a
;;; subclass of a graph where some edges are drawn differently or to
;;; provide a subclass of a graph with more verbose node labels. One way to
;;; achieve such inheritance is to provide one generic function for each
;;; attribute. This approach is tedious, however, given the many dozens of
;;; Graphviz attributes.  Instead, we provide a single generic function for
;;; each class of Graphviz entities, but with a particular method
;;; combination. This method combination takes the property lists of all
;;; applicable methods and removes all but the most specific entry of each
;;; key.

(defun plist-union (&rest plists)
  (alexandria:hash-table-plist
   (alexandria:plist-hash-table
    (apply #'append plists))))

(define-method-combination graphviz-attributes ()
  ((primary ()))
  `(plist-union
    ,@(loop for method in primary
            collect `(call-method ,method))))
