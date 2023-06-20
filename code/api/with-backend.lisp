;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

(defmacro with-backend (backend &body body)
  (alexandria:once-only (backend)
    `(let ((*backend* ,backend))
       (unwind-protect (progn ,@body)
         (delete-backend ,backend)))))
