;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :cl-user)

(defpackage :petalisp
  (:use :cl :optima :alexandria :fare-memoization)
  (:import-from :fiveam #:test #:is #:*test-dribble* #:in-suite #:def-suite #:signals)
  (:shadowing-import-from :trivial-garbage #:make-hash-table)
  (:shadow #:intersection #:compose)
  (:export
   #:α
   #:β
   #:->
   #:τ
   #:σ
   #:fuse
   #:repeat
   #:subspace))
