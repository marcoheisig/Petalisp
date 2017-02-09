;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; packages used in Petalisp and the list of exported functions

(in-package :cl-user)

(defpackage :petalisp
  (:use
   :closer-common-lisp
   :optima
   :alexandria
   :fare-memoization
   :anaphora)
  (:shadowing-import-from :trivial-garbage #:make-hash-table)
  (:shadow #:intersection #:compose)
  (:import-from :fiveam #:test #:is #:*test-dribble* #:in-suite #:def-suite #:signals)
  (:export
   #:α
   #:β
   #:->
   #:τ
   #:σ
   #:subspace))
