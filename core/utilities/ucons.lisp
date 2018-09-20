;;;; © 2016-2018 Marco Heisig - licensed under AGPLv3, see the file COPYING     -*- coding: utf-8 -*-

(cl:defpackage :ucons
  (:use :cl :alexandria)
  (:export
   #:ucons
   #:ucar
   #:ucdr
   #:ulist
   #:ulist*
   #:do-ulist
   #:map-ulist
   #:ulength
   #:copy-ulist
   #:copy-utree))

(in-package :ucons)

;;; ucons - unique conses
;;;
;;; Some applications benefit a lot by reusing existing cons cells instead
;;; of actual consing.  Usually this technique is called hash consing.
;;; Users of this technique are e.g. the optimizer of the computer algebra
;;; system Maxima and the theorem prover ACL2.
;;;
;;; This particular implementation is intended for use cases where
;;; performance is so critical, that even a single hash table access per
;;; cons is too expensive.  To achieve such near-optimal speed, this
;;; library does not actually provide conses, but uconses.  A ucons has not
;;; only a car and a cdr, but also a table of past users.  Furthermore, the
;;; cdr of each ucons is restricted to other uconses or NIL.  This setup
;;; has several advantages:
;;;
;;; - Checking whether a certain ucons already exists is a single lookup of
;;;   its car in the table of its cdr.
;;;
;;; - The immutability of a ucons is enforced by its defstruct definition
;;;
;;; - The compiler has reliable type information of the slots of a ucons.
;;;
;;; - Lists of uconses are neither circular, nor improper.
;;;
;;; Unfortunately there is also a painful downside of this approach.
;;; Traditional cons cells are a fundamental Lisp data type and well
;;; supported throughout the standard library.  Uconses lack this
;;; integration and require a completely new set of library functions.
;;; Furthermore it must be noted that uconses are --- except if one
;;; explicitly clears the *UCONS-LEAF-TABLE* --- a permanent memory leak.
;;;
;;; Yet if you are willing to accept these trade-offs, uconses offer some
;;; unique benefits:
;;;
;;; - their usage is little more expensive than a call to CONS. If you
;;;   include GC time, they can even be much faster.
;;;
;;; - given enough potential for structural sharing, uconses can decrease
;;;   the memory consumption of an application by orders of magnitude.
;;;
;;; - checks for structural similarity can be done in constant time.  Two
;;;   ucons trees are equal if and only if their roots are EQ.
;;;
;;; Benchmarks:
;;; (SBCL 1.3.20, X86-64 Intel i7-5500U CPU @ 2.40GHz)
;;;
;;; (bench  (list 1 2 3 4 5 6 7 8)) -> 25.77 nanoseconds
;;; (bench (ulist 1 2 3 4 5 6 7 8)) -> 38.18 nanoseconds

(declaim (hash-table *ucons-leaf-table*))
(defvar *ucons-leaf-table* (make-hash-table :test #'eq)
  "The table of all uconses whose cdr is NIL.")

(deftype ucar () 't)

(defstruct (ucons
            (:constructor make-fresh-ucons (car cdr))
            (:copier nil) ; this is the whole point, isn't it?
            (:predicate uconsp)
            (:conc-name u))
  (cdr   nil :type (or structure-object null) :read-only t)
  (car   nil :type ucar  :read-only t)
  (table nil :type (or list hash-table) :read-only nil))

(deftype ulist ()
  "A list made of UCONSes, or NIL."
  '(or ucons null))

(declaim (inline ucons)
         (notinline ucons-leaf ucons-hash ucons-list)
         (ftype (function (ucons) ucar) ucar)
         (ftype (function (ucons) ulist) ucdr)
         (ftype (function (ucar ulist) ucons) ucons)
         (ftype (function (ucar) ucons) ucons-leaf)
         (ftype (function (ucar ucons) ucons) ucons-hash ucons-list))

(defun ucons (car cdr)
  "Given a suitable CAR and CDR, return a UCONS that is EQ to all future
and past invocation of this function with the same arguments."
  (declare (type ulist cdr)
           (type ucar car)
           (optimize (speed 3) (safety 0) (debug 0)))
  (if (null cdr)
      (ucons-leaf car)
      (let ((table (utable cdr)))
        (if (listp table)
            (loop for (ucar . ucdr) #-ccl of-type #-ccl (ucar . ucons) in table
                  when (eq ucar car)
                    do (return ucdr)
                  finally (return (ucons-list car cdr)))
            (ucons-hash car cdr)))))

;;; Called for UCONSES with a CDR of NIL
(defun ucons-leaf (car)
  (declare (ucar car))
  (values
   (ensure-gethash car *ucons-leaf-table* (make-fresh-ucons car nil))))

;;; Called if the UTABLE of CDR is a hash table
(defun ucons-hash (car cdr)
  (declare (ucar car) (ucons cdr))
  (values
   (ensure-gethash car (utable cdr) (make-fresh-ucons car cdr))))

;;; Called if the UTABLE of CDR is an alist that does not contain CAR.
(defun ucons-list (car cdr)
  (declare (ucar car) (ucons cdr))
  (let ((ucons (make-fresh-ucons car cdr)))
    (prog1 ucons
      (if (< (length (utable cdr)) 16)
          (push (cons car ucons) (utable cdr))
          (let ((hash-table (alist-hash-table (utable cdr) :size 32)))
            (setf (gethash car hash-table) ucons)
            (setf (utable cdr) hash-table))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ulist creation

(defun ulist (&rest args)
  "Return the ulist associated with the supplied arguments."
  (declare (dynamic-extent args))
  (reduce #'ucons args :from-end t :initial-value nil))

(define-compiler-macro ulist (&rest args)
  (flet ((symbolic-ucons (car cdr)
           `(ucons ,car ,cdr)))
    (reduce #'symbolic-ucons args :from-end t :initial-value nil)))

(defun ulist* (&rest args)
  "Return the ulist associated with the supplied arguments, but using the
   last argument as the tail of the constructed ulist."
  (declare (dynamic-extent args))
  (labels ((%hlist* (first rest)
             (if (null rest)
                 (prog1 (the ulist first)
                   (check-type first (or ucons null)))
                 (ucons first (%hlist* (car rest) (cdr rest))))))
    (%hlist* (first args) (rest args))))

(define-compiler-macro ulist* (&rest arg-forms)
  (let* ((n (length arg-forms))
         (gensyms (loop repeat n collect (gensym "ARG"))))
    `(let* ,(mapcar #'list gensyms arg-forms)
       ,(let* ((rgensyms (reverse gensyms))
               (result-form
                 `(prog1 ,(car rgensyms)
                    (check-type ,(car rgensyms)
                                (or ucons null)))))
          (loop for gensym in (cdr rgensyms)
                do (setf result-form `(ucons ,gensym ,result-form)))
          result-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ulist iteration

(defmacro do-ulist ((var ulist &optional result) &body body)
  (check-type var symbol)
  (multiple-value-bind (forms decls)
      (parse-body body)
    (once-only (ulist)
      (with-gensyms (start)
        `(block nil
           (tagbody
              ,start
              (when ,ulist
                (let ((,var (ucar ,ulist)))
                  ,@decls
                  (tagbody ,@forms))
                (setf ,ulist (ucdr ,ulist))
                (go ,start)))
           (let ((,var nil))
             (declare (ignorable ,var))
             ,result))))))

(declaim (inline map-ulist))
(defun map-ulist (function &rest sequences)
  (declare (dynamic-extent sequences)
           (function function))
  (let ((length (reduce #'min sequences :key #'length))
        (stack-allocation-threshold 30))
    (flet ((map-ulist-with-buffer (buffer)
             (apply #'map-into buffer function sequences)
             (let ((result '()))
               (loop for index from (1- length) downto 0 do
                 (setf result (ucons (aref buffer index) result)))
               result)))
      (if (<= length stack-allocation-threshold)
          (let ((buffer (make-array stack-allocation-threshold)))
            (declare (dynamic-extent buffer))
            (map-ulist-with-buffer buffer))
          (let ((buffer (make-array length)))
            (map-ulist-with-buffer buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; other ulist utilities

(defun ulength (ulist)
  "Return the length of the given ulist."
  (declare (ulist ulist) (optimize speed))
  (loop for elt = ulist then (ucdr elt)
        while elt count t))

(defun copy-ulist (ulist)
  "Return a list of the elements of ULIST."
  (declare (ulist ulist))
  (loop for elt = ulist then (ucdr elt)
        while elt collect (ucar elt)))

(defun copy-utree (ulist)
  "Return a tree of the same shape as ULIST, but where all occuring ulists
   have been converted to lists."
  (declare (ulist ulist))
  (loop for elt = ulist then (ucdr elt)
        while elt
        for car = (ucar elt)
        collect (if (uconsp car)
                    (copy-utree car)
                    car)))

(defmethod print-object ((ulist ucons) stream)
  (cond (*print-pretty*
         (let ((list (ucons:copy-ulist ulist)))
           (pprint-logical-block (stream list :prefix "[" :suffix "]")
             (pprint-fill stream list nil))))
        (t
         (write-string "[" stream)
         (loop while ulist do
           (write (ucar ulist) :stream stream)
           (when (ucdr ulist)
             (write-string " " stream))
           (setf ulist (ucdr ulist)))
         (write-string "]" stream))))
