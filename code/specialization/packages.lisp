;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(cl:in-package #:common-lisp-user)

(defpackage #:petalisp.specialization
  (:use
   #:common-lisp)
  (:export
   #:specialize
   #:f32
   #:f64
   #:c64
   #:c128
   #:u1
   #:u2
   #:u4
   #:u8
   #:u16
   #:u32
   #:u64
   #:s8
   #:s16
   #:s32
   #:s64))
