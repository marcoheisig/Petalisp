;;;; © 2016-2022 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.utilities
  (:use #:common-lisp)
  (:export

   ;; documentation.lisp
   #:document-compiler-macro
   #:document-compiler-macros
   #:document-function
   #:document-functions
   #:document-method-combination
   #:document-method-combinations
   #:document-setf-expander
   #:document-setf-expanders
   #:document-structure
   #:document-structures
   #:document-variable
   #:document-variables

   ;; bitfield.lisp
   #:define-bitfield

   ;; defalias.lisp
   #:defalias

   ;; queue.lisp
   #:queue
   #:queuep
   #:make-queue
   #:queue-enqueue
   #:queue-dequeue

   ;; wsdeque.lisp
   #:wsdeque
   #:wsdequep
   #:make-wsdeque
   #:wsdeque-push
   #:wsdeque-pop
   #:wsdeque-steal

   ;; extended-euclid.lisp
   #:extended-euclid

   ;; identical.lisp
   #:identical

   ;; memoization.lisp
   #:with-memoization
   #:with-multiple-value-memoization
   #:with-hash-table-memoization
   #:with-multiple-value-hash-table-memoization
   #:with-vector-memoization
   #:with-multiple-value-vector-memoization

   ;; prime-factors.lisp
   #:prime-factors
   #:primep

   ;; weak-set.lisp
   #:weak-set
   #:weak-set-p
   #:make-weak-set
   #:map-weak-set
   #:weak-set-size
   #:weak-set-add

   ;; with-collectors.lisp
   #:with-collectors

   ;; number-of-cpus.lisp
   #:number-of-cpus

   ;; topological-sort.lisp
   #:topological-sort

   ;; graph-coloring.lisp
   #:make-cgraph
   #:cgraph-add-conflict
   #:cgraph-coloring))
