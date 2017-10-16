;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defvar *hcons-table* (make-hash-table :test #'eql :size 2000)
  "A mapping from hcons hashes (non-negative-fixnums) to lists of hconses.")

(defstruct (hcons
            (:constructor %make-hcons (car cdr hash))
            (:copier nil) ; this is the whole point, isn't it?
            (:predicate hconsp))
  (car nil :read-only t)
  (cdr nil :read-only t)
  (hash nil :type non-negative-fixnum :read-only t))

(declaim (inline hcons))
(defun hcons (car cdr)
  "Return the unique hcons whose car and cdr are EQUAL to the given CAR and CDR,
   respectively."
  (with-unsafe-optimizations
    (flet ((hash (x)
             (if (hconsp x)
                 (hcons-hash x)
                 (sxhash x)))
           (mix (hash-1 hash-2)
             (declare (non-negative-fixnum hash-1 hash-2))
             (let ((cons (cons hash-1 hash-2)))
               (declare (type (cons non-negative-fixnum non-negative-fixnum) cons)
                        (dynamic-extent cons))
               (sxhash cons))))
      (let ((key (mix (hash car) (hash cdr))))
        (if-let ((lookup (find-if (lambda (hcons)
                                    (declare (hcons hcons))
                                    (and (equal car (hcons-car hcons))
                                         (equal cdr (hcons-cdr hcons))))
                                  (the list (gethash key *hcons-table*)))))
          (values lookup nil)
          (let ((new-hcons (%make-hcons car cdr key)))
            (push new-hcons (gethash key *hcons-table*))
            (values new-hcons t)))))))

(defun hlist (&rest args)
  (declare (dynamic-extent args))
  (reduce #'hcons args :from-end t :initial-value nil))

(defmethod print-object ((hcons hcons) stream)
  (prog1 hcons
    (write-string "[" stream)
    (loop
      (print-object (hcons-car hcons) stream)
      (etypecase (hcons-cdr hcons)
        (null (return))
        (hcons
         (write-string " " stream)
         (setf hcons (hcons-cdr hcons)))
        (t
         (write-string " . " stream)
         (print-object (hcons-cdr hcons) stream)
         (return))))
    (write-string "]" stream)))

(defun hcons-reader (stream char)
  (declare (ignore char))
  (let ((list (read-delimited-list #\] stream t)))
    (reduce #'hcons list :from-end t :initial-value nil)))

(set-macro-character #\[ #'hcons-reader)
(set-macro-character #\] (get-macro-character #\) nil))
