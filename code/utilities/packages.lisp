;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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

   ;; queue.lisp
   #:queue
   #:queuep
   #:make-queue
   #:queue-enqueue
   #:queue-dequeue

   ;; extended-euclid.lisp
   #:extended-euclid

   ;; prime-factors.lisp
   #:prime-factors
   #:primep

   ;; with-collectors.lisp
   #:with-collectors

   ;; number-of-cpus.lisp
   #:number-of-cpus

   ;; graph-coloring.lisp
   #:make-cgraph
   #:cgraph-ensure-cnode
   #:cgraph-add-conflict
   #:cgraph-coloring

   ;; karmarkar-karp.lisp
   #:karmarkar-karp))
