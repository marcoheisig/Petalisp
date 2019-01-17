;;;; © 2016-2019 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package :petalisp-api)

(defmacro ~ (&rest tilde-separated-range-designators)
  (if (null tilde-separated-range-designators)
      `(make-shape)
      (let ((range-designators
              (split-sequence:split-sequence '~ tilde-separated-range-designators)))
        `(make-shape
          ,@(loop for range-designator in range-designators
                  collect `(range ,@range-designator))))))

(trivia:defpattern ~ (&rest tilde-separated-range-designators)
  (if (null tilde-separated-range-designators)
      `(shape)
      (let ((range-designators
              (split-sequence:split-sequence '~ tilde-separated-range-designators)))
        `(shape
          ,@(loop for range-designator in range-designators
                  collect `(range ,@range-designator))))))
