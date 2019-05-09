;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.type-codes)

(deftype character-designator ()
  '(or (vector character 1) character))

(deftype string-designator ()
  '(or character symbol string))

(deftype radix ()
  '(integer 2 36))

(deftype character-code ()
  '(integer 0 (#.char-code-limit)))

(flet ((inference (characters)
         (dolist (character characters)
           (check-type-code character character))
         (type-code-from-type-specifier 't)))
  (define-type-inference-rule char= (&rest characters) (inference characters))
  (define-type-inference-rule char/= (&rest characters) (inference characters))
  (define-type-inference-rule char< (&rest characters) (inference characters))
  (define-type-inference-rule char> (&rest characters) (inference characters))
  (define-type-inference-rule char<= (&rest characters) (inference characters))
  (define-type-inference-rule char>= (&rest characters) (inference characters))
  (define-type-inference-rule char-equal (&rest characters) (inference characters))
  (define-type-inference-rule char-not-equal (&rest characters) (inference characters))
  (define-type-inference-rule char-lessp (&rest characters) (inference characters))
  (define-type-inference-rule char-greaterp (&rest characters) (inference characters))
  (define-type-inference-rule char-not-greaterp (&rest characters) (inference characters))
  (define-type-inference-rule char-not-lessp (&rest characters) (inference characters)))

(define-type-inference-rule character (character-designator)
  (check-type-code character-designator character-designator)
  (type-code-from-type-specifier 'character))

(define-predicate-type-inference-rule characterp character)

(flet ((inference (character)
         (check-type-code character character)
         (type-code-from-type-specifier 't)))
  (define-type-inference-rule alpha-char-p (character) (inference character))
  (define-type-inference-rule alphanumericp (character) (inference character))
  (define-type-inference-rule graphic-char-p (character) (inference character))
  (define-type-inference-rule standard-char-p (character) (inference character))
  (define-type-inference-rule upper-case-p (character) (inference character))
  (define-type-inference-rule lower-case-p (character) (inference character))
  (define-type-inference-rule both-case-p (character) (inference character)))

(define-type-inference-rule digit-char (weight &optional (radix (type-code-of 10)))
  (check-type-code weight (integer 0 *))
  (check-type-code radix radix)
  (type-code-from-type-specifier '(or character null)))

(define-type-inference-rule digit-char-p (char &optional (radix (type-code-of 10)))
  (check-type-code char character)
  (check-type-code radix radix)
  (type-code-from-type-specifier '(or (integer 0 35) null)))

(define-type-inference-rule char-upcase (character)
  (check-type-code character character)
  (type-code-from-type-specifier 'character))

(define-type-inference-rule char-downcase (character)
  (check-type-code character character)
  (type-code-from-type-specifier 'character))

(define-type-inference-rule char-code (character)
  (check-type-code character character)
  (type-code-from-type-specifier 'character-code))

(define-type-inference-rule char-int (character)
  (check-type-code character character)
  (type-code-from-type-specifier '(integer 0 *)))

(define-type-inference-rule code-char (character-code)
  (check-type-code character-code character-code)
  (type-code-from-type-specifier 'character))

(define-type-inference-rule char-name (character)
  (check-type-code character character)
  (type-code-from-type-specifier '(or string null)))

(define-type-inference-rule name-char (name)
  (check-type-code name string-designator)
  (type-code-from-type-specifier '(or character null)))
