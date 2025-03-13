(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.benchmarks
  (:use #:common-lisp #:petalisp)
  (:export
   #:*benchmarks*
   #:print-benchmark-table))
