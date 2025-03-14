(in-package #:petalisp.test-suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A Test Framework, Inspired by 1am

;; A list of the names of all tests defined with the TEST macro.
(defparameter *tests* '())

;; The value of *RANDOM-STATE* when the test suite encountered the most
;; recent error, or NIL, if there have been no recent errors.
(defvar *failed-random-state* nil)

;; The number of tests that have been run so far.
(defvar *test-count*)

;; The number of successful checks that have been performed so far.
(defvar *pass-count*)

;; The number of checks that have been performed in the current test.
(defvar *check-count* 0)

(defun call-with-random-state (thunk)
  (let ((*random-state*
          (if (not *failed-random-state*)
              (load-time-value (make-random-state t))
              *failed-random-state*)))
    (setf *failed-random-state* (make-random-state *random-state*))
    (multiple-value-prog1 (funcall thunk)
      (setf *failed-random-state* nil))))

(defmacro with-random-state (&body body)
  `(call-with-random-state (lambda () ,@body)))

(defun call-with-test-harness (thunk)
  (if (and (boundp '*test-count*)
           (boundp '*pass-count*))
      (funcall thunk)
      (let* ((*test-count* 0)
             (*pass-count* 0))
        (with-random-state (funcall thunk))
        (report *test-count* *pass-count*))))

(defmacro with-test-harness (&body body)
  `(call-with-test-harness (lambda () ,@body)))

(defun report (test-count pass-count)
  (format t "~&Success: ~s test~:p, ~s check~:p.~%" test-count pass-count)
  (finish-output))

(defmacro define-test (name &body body)
  "Define a test function and add it to *TESTS*."
  `(prog1 ',name
     (defun ,name ()
       (declare (optimize (debug 3)))
       (with-test-harness
         (enter-test ',name)
         ,@body))
     (pushnew ',name *tests*)))

(defun enter-test (test-name)
  (incf *test-count*)
  (setf *check-count* 0)
  (format t "~&~S" test-name)
  (finish-output))

(defun pass ()
  (incf *pass-count*)
  (when (zerop (logand *check-count* (incf *check-count*)))
    (write-char #\.))
  (values))

(defmacro is (test-form)
  "Assert that TEST-FORM evaluates to true."
  `(progn
     (assert ,test-form)
     (pass)))

(defun expect-condition (expected thunk)
  (block nil
    (flet ((handler (condition)
             (cond ((typep condition expected)
                    (pass)
                    (return))
                   (t (error "Expected to signal ~s, but got ~s:~%~a"
                             expected (type-of condition) condition)))))
      (handler-bind ((condition #'handler))
        (funcall thunk)))
    (error "Expected to signal ~s, but got nothing." expected)))

(defmacro signals (condition &body body)
  "Assert that `body' signals a condition of type `condition'."
  `(expect-condition ',condition (lambda () ,@body)))

(defun all-tests ()
  *tests*)

;;; This function catches particularly hideous bugs - exported symbols with
;;; no attached definition.  In case some exported symbols are
;;; intentionally without definition, they can be excluded via the SKIP
;;; argument.
(defun check-package (package &key skip)
  (do-external-symbols (symbol package)
    (unless (member symbol skip)
      (unless (symbol-defined-p symbol)
        (error "~@<The package ~A exports the symbol ~A, ~
                     which does not have an attached definition.~:@>"
               package (symbol-name symbol))))))

(defun symbol-defined-p (symbol)
  (or (find-class symbol nil)
      (boundp symbol)
      (fboundp symbol)
      (fboundp `(setf ,symbol))
      (macro-function symbol)
      (special-operator-p symbol)
      (type-specifier-p symbol)))

(defun type-specifier-p (object)
  (ignore-errors (typep 42 object) t))
