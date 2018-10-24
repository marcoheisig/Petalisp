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
   #:umapcar
   #:ulength
   #:copy-ulist
   #:copy-utree
   #:*root-table*
   #:make-root-table))

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
;;; explicitly clears the *ROOT-TABLE* --- a permanent memory leak.
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
;;;   ucons trees are equal if and only if their roots are EQL.
;;;
;;; Benchmarks:
;;; (SBCL 1.3.20, X86-64 Intel i7-5500U CPU @ 2.40GHz)
;;;
;;; (bench  (list 1 2 3 4 5 6 7 8)) -> 25.77 nanoseconds
;;; (bench (ulist 1 2 3 4 5 6 7 8)) -> 38.18 nanoseconds

(defstruct (ucons
            (:constructor make-fresh-ucons (car cdr))
            (:copier nil) ; This is the whole point, isn't it?
            (:predicate uconsp)
            (:conc-name u))
  (cdr   nil :read-only t   :type (or structure-object null))
  (car   nil :read-only t   :type t )
  (table nil :read-only nil :type (or list hash-table)))

(deftype ulist ()
  "A list made of UCONSes, or NIL."
  '(or ucons null))

(declaim (inline ucons)
         (notinline ucons-leaf ucons-hash ucons-list)
         (ftype (function (ucons)   t)     ucar)
         (ftype (function (ucons)   ulist) ucdr)
         (ftype (function (t ulist) ucons) ucons)
         (ftype (function (t)       ucons) ucons-leaf)
         (ftype (function (t ucons) ucons) ucons-hash ucons-list))

(defun ucons (car cdr)
  "Given a suitable CAR and CDR, return a UCONS that is EQL to all future
and past invocation of this function with the same arguments."
  (declare (type ulist cdr)
           (optimize (safety 0) (debug 0)))
  (if (null cdr)
      (ucons-leaf car)
      (let ((table (utable cdr)))
        (if (listp table)
            (loop for entry of-type cons in table do
              (when (eql (car entry) car)
                (return (cdr entry)))
                  finally (return (ucons-list car cdr)))
            (ucons-hash car cdr)))))

;;; Called if the UTABLE of CDR is a hash table
(defun ucons-hash (car cdr)
  (declare (ucons cdr))
  (values
   (ensure-gethash car (utable cdr) (make-fresh-ucons car cdr))))

;;; Called if the UTABLE of CDR is an alist that does not contain CAR.
(defun ucons-list (car cdr)
  (declare (ucons cdr))
  (let ((ucons (make-fresh-ucons car cdr)))
    (prog1 ucons
      (if (< (length (utable cdr)) 16)
          (push (cons car ucons) (utable cdr))
          (let ((hash-table (alist-hash-table (utable cdr) :size 32)))
            (setf (gethash car hash-table) ucons)
            (setf (utable cdr) hash-table))))))

;;; Lookup of root nodes, i.e., uconses whose cdr is NIL.

(defstruct (root-table (:copier nil)
                       (:predicate nil))
  (cache (make-hash-table) :read-only t :type hash-table)
  (small-integer-cache
   (let ((array (make-array 32)))
     (dotimes (index 32 array)
       (setf (aref array index)
             (make-fresh-ucons index nil))))
   :read-only t
   :type (simple-array ucons (32))))

(declaim (root-table *root-table*))
(defvar *root-table* (make-root-table)
  "The table of all uconses whose cdr is NIL.")

(defun ucons-leaf (car)
  (if (typep car '(integer 0 (32)))
      ;; Go to the small integer cache
      (aref (root-table-small-integer-cache *root-table*) car)
      ;; Else, go to the slightly slower general cache.
      (let ((cache (root-table-cache *root-table*)))
        (values
         (ensure-gethash car cache (make-fresh-ucons car nil))))))

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
  (labels ((aux (first rest)
             (if (null rest)
                 (prog1 (the ulist first)
                   (check-type first (or ucons null)))
                 (ucons first (aux (car rest) (cdr rest))))))
    (aux (first args) (rest args))))

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

(declaim (inline umapcar))
(defun umapcar (function &rest sequences)
  "Return an ulist containing the results of applying FUNCTION to
successive elements of the supplied sequences.  The resulting ulist is as
long as the shortest supplied sequence."
  (declare (dynamic-extent sequences)
           (function function))
  (let ((length (reduce #'min sequences :key #'length))
        (stack-allocation-threshold 11))
    (flet ((ulist-from-buffer (buffer)
             (let ((ulist '()))
               (loop for index from (1- length) downto 0 do
                 (setf ulist (ucons (aref buffer index) ulist)))
               ulist)))
      (if (<= length stack-allocation-threshold)
          (let ((buffer (make-array stack-allocation-threshold)))
            (declare (dynamic-extent buffer))
            (apply #'map-into buffer function sequences)
            (ulist-from-buffer buffer))
          (let ((buffer (make-array length)))
            (apply #'map-into buffer function sequences)
            (ulist-from-buffer buffer))))))

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

(defun copy-utree (utree)
  "Return a tree of the same shape as UTREE, but where all occuring ulists
have been converted to lists."
  (declare (ulist utree))
  (loop for elt = utree then (ucdr elt)
        while elt
        for car = (ucar elt)
        collect (if (uconsp car)
                    (copy-utree car)
                    car)))

(defmethod print-object ((ulist ucons) stream)
  (cond (*print-pretty*
         (let ((list (ucons:copy-ulist ulist)))
           (pprint-logical-block (stream list :prefix "[" :suffix "]")
             (pprint-linear stream list nil))))
        (t
         (write-string "[" stream)
         (loop while ulist do
           (write (ucar ulist) :stream stream)
           (when (ucdr ulist)
             (write-string " " stream))
           (setf ulist (ucdr ulist)))
         (write-string "]" stream))))
