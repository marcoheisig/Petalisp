;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :cl-user)

(defpackage :petalisp
  (:use :closer-common-lisp :alexandria :iterate)
  ;; support for weak hashtables
  (:shadowing-import-from :trivial-garbage #:make-hash-table)
  ;; shadow CL:INTERSECTION with a generic method
  (:shadow #:intersection)
  ;; Petalisp imports only a small subset of fiveam functionality
  (:import-from :fiveam #:is #:is-true #:is-false #:signals #:for-all)
  ;; support for pattern matching
  (:import-from :optima #:match #:ematch #:cmatch)
  ;; LOOP no more
  (:import-from :iterate #:iterate)
  ;; The Petalisp API
  (:export
   #:α
   #:β
   #:->
   #:τ
   #:σ
   #:subspace))
