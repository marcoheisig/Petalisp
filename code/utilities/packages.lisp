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

   ;; extended-euclid.lisp
   #:extended-euclid

   ;; powers-of-two.lisp
   #:flp2
   #:clp2

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
   #:karmarkar-karp

   ;; with-pinned-objects.lisp
   #:with-pinned-objects
   #:with-pinned-objects*))
