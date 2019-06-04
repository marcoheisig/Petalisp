;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.specialization)

(deftype f32 () 'single-float)
(deftype f64 () 'double-float)

(deftype c64 () '(complex short-float))
(deftype c128 () '(complex double-float))

(deftype u1 () 'bit)
(deftype u2 () '(unsigned-byte 2))
(deftype u4 () '(unsigned-byte 4))
(deftype u8 () '(unsigned-byte 8))
(deftype u16 () '(unsigned-byte 16))
(deftype u32 () '(unsigned-byte 32))
(deftype u64 () '(unsigned-byte 64))

(deftype s8 () '(signed-byte 8))
(deftype s16 () '(signed-byte 16))
(deftype s32 () '(signed-byte 32))
(deftype s64 () '(signed-byte 64))
