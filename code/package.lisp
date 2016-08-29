;;; © 2016 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :cl-user)

(defpackage :petalisp
  (:use :cl :optima :alexandria :fare-memoization)
  (:shadow #:equalp #:intersection #:compose)
  (:export
   #:α
   #:β
   #:fuse
   #:repeat
   #:<-
   #:subspace))
