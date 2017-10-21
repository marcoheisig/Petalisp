;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(deftype ucons-car () '(or fixnum symbol character ucons))

(deftype ulist () '(or ucons null))

(defstruct (ucons
            (:constructor %ucons (car cdr))
            (:copier nil) ; this is the whole point, isn't it?
            (:predicate uconsp))
  (cdr   nil :type (or ucons null) :read-only t)
  (car   nil :type ucons-car       :read-only t)
  ;; TABLE tracks all uconses whose cdr is this cons
  (table nil :type (or list hash-table) :read-only nil))

(declaim (hash-table *ucons-root-table*))
(defvar *ucons-root-table* (make-hash-table :test #'eql))

(declaim (inline ucons))
(defun ucons (car cdr)
  "Given a CAR that is either a number, a symbol, a character or a ucons
   and a CDR that is either NIL, or a ucons, return a UCONS that is EQ to
   all future and past invocation of this function with the same
   arguments."
  (declare (type (or null ucons) cdr)
           (type ucons-car car))
  (flet ((hash-table-lookup (hash-table)
           (declare (hash-table hash-table))
           (or (gethash car hash-table)
               (setf (gethash car hash-table)
                     (%ucons car cdr))))
         (alist-lookup (alist)
           (declare (list alist))
           (or (cdr (assoc car alist))
               (let ((ucons (%ucons car cdr)))
                 (cond
                   ((> (length alist) 5)
                    (setf (ucons-table cdr)
                          (alist-hash-table alist :test #'eql :size 16))
                    (setf (gethash car (ucons-table cdr)) ucons))
                   (t
                    (push (cons car ucons)
                          (ucons-table cdr))))
                 ucons))))
    (if (null cdr)
        (hash-table-lookup *ucons-root-table*)
        (let ((table (ucons-table cdr)))
          (etypecase table
            (list (alist-lookup table))
            (hash-table (hash-table-lookup table)))))))

(defun ulist (&rest args)
  "Return the ulist associated with the supplied arguments."
  (declare (dynamic-extent args))
  (labels ((%list (first rest)
             (if (null rest)
                 (ucons first nil)
                 (ucons first (%list (car rest) (cdr rest))))))
    (%list (first args) (rest args))))

(define-compiler-macro ulist (&whole whole &rest arg-forms)
  (if (> (length arg-forms) 15)
      whole
      (let ((gensyms
              (loop for arg-form in arg-forms
                    collect (gensym "ARG"))))
        `(let* ,(mapcar #'list gensyms arg-forms)
           ,(let (form)
              (loop for gensym in (reverse gensyms)
                    do (setf form `(ucons ,gensym ,form)))
              form)))))

(defun ulist* (&rest args)
  "Return the ulist associated with the supplied arguments, but using the
   last argument as the tail of the constructed ulist."
  (declare (dynamic-extent args))
  (labels ((%hlist* (first rest)
             (if (null rest)
                 (prog1 first
                   (check-type first (or ucons null)))
                 (ucons first (%hlist* (car rest) (cdr rest))))))
    (%hlist* (first args) (rest args))))

(define-compiler-macro ulist* (&whole whole &rest arg-forms)
  (if (> (length arg-forms) 15)
      whole
      (let ((gensyms
              (loop for arg-form in arg-forms
                    collect (gensym "ARG"))))
        `(let* ,(mapcar #'list gensyms arg-forms)
           ,(let* ((rgensyms (reverse gensyms))
                   (form `(prog1 ,(car rgensyms)
                            (check-type ,(car rgensyms)
                                        (or ucons null)))))
              (loop for gensym in (cdr rgensyms)
                    do (setf form `(ucons ,gensym ,form)))
              form)))))

(defun ulength (ulist)
  "Return the length of the given ulist."
  (declare (ulist ulist) (optimize speed))
  (loop while ulist count t
        do (setf ulist (ucons-cdr ulist))))
