;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

(defstruct (request
            (:predicate %requestp)
            (:copier nil)
            (:constructor make-request))
  (lock (bordeaux-threads:make-lock) :read-only t)
  (cvar (bordeaux-threads:make-condition-variable) :read-only t)
  (done nil :type boolean))

(defmethod requestp ((request request))
  t)

(defmethod request-wait ((request request))
  (with-slots (lock cvar done) request
    (unless done
      (bordeaux-threads:with-lock-held (lock)
        (loop until done do
          (bordeaux-threads:condition-wait cvar lock))))))

(defmethod request-completedp ((request request))
  (request-done request))
