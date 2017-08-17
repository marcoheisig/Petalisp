;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; packages used in Petalisp and the list of exported functions

(in-package :cl-user)

(defpackage :petalisp
  (:use
   :closer-common-lisp
   :alexandria
   :fare-memoization
   :anaphora)
  (:shadowing-import-from :trivial-garbage #:make-hash-table)
  (:shadow #:intersection #:compose)
  (:import-from :fiveam
                #:test #:is #:is-true #:is-false
                #:*test-dribble* #:in-suite #:def-suite #:signals
                #:for-all #:gen-integer #:gen-float)
  (:import-from :optima
                #:match #:ematch #:cmatch)
  (:export
   #:α
   #:β
   #:->
   #:τ
   #:σ
   #:subspace))

(in-package :petalisp)

(def-suite petalisp :description "All Petalisp related tests.")
