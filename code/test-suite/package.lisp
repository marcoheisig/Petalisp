;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :common-lisp-user)

(defpackage :petalisp-test-suite
  (:use
   :alexandria
   :common-lisp
   :petalisp-internals
   :petalisp-test-suite/examples/jacobi
   :petalisp-test-suite/examples/red-black-gauss-seidel
   :petalisp-test-suite/examples/linear-algebra)
  (:import-from :fiveam
                #:*on-error* #:*on-failure*
                #:in-suite #:in-suite* #:test
                #:is #:is-true #:is-false #:signals #:finishes #:for-all)
  (:shadowing-import-from :petalisp-internals #:intersection #:union))
