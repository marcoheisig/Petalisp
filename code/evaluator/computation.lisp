;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(define-class computation (immediate)
  ((state :initform :scheduled
           :type (member :scheduled :allocated :finished :deleted)
           :accessor state)
   (storage :initform nil
            :accessor storage)
   (lock :initform (make-recursive-lock))
   (cvar :initform (make-condition-variable)))
  (:documentation
   "Computations are used as sources or targets of computations. Each
   computation runs through four states:

   :scheduled - the initial state
   :allocated - storage is bound to an uninitialized array
   :finished  - the storage array contains the results of the computation
   :deleted   - storage is NIL, the computation is not used anymore"))

(defmethod (setf state) :around ((computation computation) new-state)
  (with-recursive-lock-held ((lock computation))
    (let* ((current-state (state computation))
           (expected-state (case current-state
                             (:scheduled :allocated)
                             (:allocated :finished)
                             (:deleted nil))))
      (if (eq new-state expected-state)
          (call-next-method)
          (simple-program-error
           "Invalid state change from ~A to ~A."
           current-state new-state)))))

(defmethod storage :around ((computation computation))
  (with-recursive-lock-held ((lock computation))
    (let ((state (state computation)))
      (ecase state
        ((:allocated :finished)
         (call-next-method))
        ((:scheduled :deleted)
         (simple-program-error
          "Access to the storage of a ~A computation."
          state))))))

(defmethod (setf storage) :around ((computation computation) value)
  (with-recursive-lock-held ((lock computation))
    (with-accessors ((storage storage) (state state)) computation
      (etypecase value
        (null
         (unless (eq state :finished)
           (simple-program-error
            "Tried to remove the storage of an array that is ~A."))
         (call-next-method)
         (setf state :deleted))
        (t
         (unless (eq state :scheduled)
           (simple-program-error
            "Tried to assign memory to a computation that is already ~A."
            state))
         (call-next-method)
         (setf state :allocated))))))

(defmethod wait-for-completion ((computation computation))
  (with-recursive-lock-held ((lock computation))
    (iterate
      (for state = (state computation))
      (when (eq state :deleted)
        (simple-program-error
         "Waiting for the completion of an already deleted computation."))
      (until (eq state :finished))
      (condition-wait (cvar computation) (lock computation)))))

(define-class strided-array-computation (strided-array computation) ())

(defmethod depetalispify ((object strided-array-computation))
  (wait-for-completion object)
  (storage object))
