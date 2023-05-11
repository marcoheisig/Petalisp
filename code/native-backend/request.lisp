;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defstruct (request
            (:predicate %requestp)
            (:copier nil)
            (:constructor make-request
             (countdown &aux (cell (list countdown)))))
  (cell nil :type cons :read-only t)
  (lock (bordeaux-threads:make-lock) :read-only t)
  (cvar (bordeaux-threads:make-condition-variable) :read-only t))

(defmethod requestp ((request request))
  t)

(defmethod request-wait ((request request))
  (with-slots (cell lock cvar) request
    (unless (zerop (car cell))
      (bordeaux-threads:with-lock-held (lock)
        (loop until (zerop (car cell)) do
          (bordeaux-threads:condition-wait cvar lock))))))

(defmethod request-completedp ((request request))
  (with-slots (cell) request
    (zerop (car cell))))
