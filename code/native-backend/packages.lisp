(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.native-backend

  (:use
   #:common-lisp
   #:petalisp.core
   #:petalisp.ir
   #:petalisp.codegen)

  (:import-from
   #:petalisp
   #:make-native-backend)

  (:shadow #:petalisp.core #:request #:backend)

  (:import-from #:petalisp.ir #:readers #:writers #:sources #:targets)

  (:export
   #:make-native-backend))
