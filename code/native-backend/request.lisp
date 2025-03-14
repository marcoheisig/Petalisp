(in-package #:petalisp.native-backend)

(defstruct (request
            (:predicate %requestp)
            (:copier nil)
            (:constructor make-request))
  (lock (bordeaux-threads-2:make-lock) :read-only t)
  (cvar (bordeaux-threads-2:make-condition-variable) :read-only t)
  (done nil :type boolean))

(defmethod requestp ((request request))
  t)

(defmethod request-wait ((request request))
  (with-slots (lock cvar done) request
    (unless done
      (bordeaux-threads-2:with-lock-held (lock)
        (loop until done do
          (bordeaux-threads-2:condition-wait cvar lock))))))

(defmethod request-completedp ((request request))
  (request-done request))
