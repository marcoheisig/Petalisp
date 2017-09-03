;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :cl-user)

(defpackage :petalisp
  (:use :closer-common-lisp :alexandria)
  ;; support for weak hashtables
  (:shadowing-import-from :trivial-garbage #:make-hash-table)
  ;; shadow CL:INTERSECTION with a generic method
  (:shadow #:intersection)
  ;; Petalisp imports only a small subset of fiveam functionality
  (:import-from :fiveam #:is #:is-true #:is-false #:signals #:for-all)
  ;; support for pattern matching
  (:import-from :optima #:match #:ematch #:cmatch)
  ;; LOOP no more
  (:import-from :iterate
                #:defsynonym #:dsetq #:declare-variables
                #:defmacro-clause #:defmacro-driver #:defclause-sequence
                #:initially #:after-each #:finally #:finally-protected
                #:else #:if-first-time #:first-iteration-p #:first-time-p
                #:finish #:leave #:next-iteration #:next #:terminate
                #:repeat #:for #:as #:generate #:generating #:in
                #:sum #:summing #:multiply #:multiplying
                #:maximize #:minimize #:maximizing #:minimizing #:counting
                #:always #:never #:thereis #:finding #:collect #:collecting
                #:with #:while #:until #:adjoining #:nconcing #:appending
                #:nunioning #:unioning #:reducing #:accumulate #:accumulating)
  ;; The Petalisp API
  (:export
   #:α
   #:β
   #:->
   #:τ
   #:σ
   #:subspace))
