;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(cl:defpackage :petalisp-ir
  (:shadowing-import-from :petalisp #:set-difference)
  (:use :closer-common-lisp :alexandria :petalisp)
  (:export
   #:ir-node
   #:kernel
   #:make-kernel
   #:buffer
   #:make-buffer
   #:ir-from-strided-arrays
   #:compute-buffer-table
   #:compute-kernels
   #:shape
   #:body
   #:inputs
   #:outputs
   #:pref
   #:preduce
   #:pcall))
