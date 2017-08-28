;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; packages used in Petalisp and the list of exported functions

(in-package :cl-user)

(defpackage :petalisp
  (:use :closer-common-lisp :alexandria :fare-memoization :anaphora)
  ;; support for weak hashtables
  (:shadowing-import-from :trivial-garbage #:make-hash-table)
  ;; shadow ALEXANDRIA:COMPOSE and CL:INTERSECTION with generic methods
  (:shadow #:intersection #:compose)
  ;; Petalisp imports only a small subset of fiveam functionality
  (:import-from :fiveam #:is #:is-true #:is-false #:signals #:for-all)
  ;; support for pattern matching
  (:import-from :optima #:match #:ematch #:cmatch)
  ;; The Petalisp API
  (:export
   #:α
   #:β
   #:->
   #:τ
   #:σ
   #:subspace))
