;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

;;; ucons - unique conses
;;;
;;; Some applications benefit a lot by reusing existing cons cells instead
;;; of actual consing. Usually this technique is called hash consing.
;;; Users of this technique are e.g. the optimizer of the computer algebra
;;; system Maxima and the theorem prover ACL2.
;;;
;;; This particular implementation is intended for use cases where
;;; performance is so critical, that even a single hash table access is too
;;; expensive. To achieve such near-optimal speed, this implementation does
;;; not actually provide conses, but uconses. A ucons has not only a car
;;; and a cdr, but also a table of past users. Furthermore, the cdr of each
;;; ucons is restricted to other uconses or NIL. This setup has several
;;; advantages:
;;;
;;; - The check whether a certain ucons already exists is a single lookup
;;;   of its car in the table of its cdr.
;;;
;;; - The immutability of the car and cdr of a ucons is enforced by the
;;;   defstruct definition of ucar.
;;;
;;; - A compiler has reliable type information of the slots of a ucons.
;;;
;;; - Lists of uconses are neither circular, nor improper.
;;;
;;; Unfortunately there is also a painful downside of this
;;; approach. Traditional cons cells are a fundamental Lisp data type and
;;; well supported throughout the standard library. Uconses lack this
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
;;; - checks for structural similarity can be done in constant time. Two
;;;   ucons trees are equal if and only if their root uconses are EQ.
;;;
;;; Benchmarks:
;;; (SBCL 1.3.20, X86-64 Intel i7-5500U CPU @ 2.40GHz)
;;;
;;; (bench  (list 1 2 3 4 5 6 7 8)) -> 25.77 nanoseconds
;;; (bench (ulist 1 2 3 4 5 6 7 8)) -> 38.18 nanoseconds

(deftype ucar ()
  "The type of all elements that may appear as the UCAR of a UCONS."
  ;; the type of things you can reasonably compare with EQ
  '(or fixnum symbol function character ucons))

(deftype ulist ()
  "A list made of UCONSes, or NIL."
  '(or ucons null))

(defstruct (ucons
            (:constructor make-fresh-ucons (car cdr))
            (:copier nil) ; this is the whole point, isn't it?
            (:predicate uconsp))
  (cdr   nil :type ulist :read-only t)
  (car   nil :type ucar  :read-only t)
  (table nil :type (or list hash-table) :read-only nil))

;;; provide classical slot readers like UCAR and UCADDR
(macrolet
    ((define-ucxr-accessors ()
       (let (ucxr-forms)
         (flet ((add-ucxr-form (&rest characters)
                  (let ((name (intern (format nil "UC~{~C~}R" characters)))
                        (body 'x))
                    (dolist (char (reverse characters))
                      (ecase char
                        (#\A (setf body `(ucons-car ,body)))
                        (#\D (setf body `(ucons-cdr ,body)))))
                    (push `(defun ,name (x) (declare (ucons x)) ,body) ucxr-forms)
                    (push `(declaim (inline ,name)) ucxr-forms))))
           (map-product #'add-ucxr-form #1='(#\A #\D))
           (map-product #'add-ucxr-form #1# #1#)
           (map-product #'add-ucxr-form #1# #1# #1#))
         `(progn ,@ucxr-forms))))
  (define-ucxr-accessors))

(declaim (hash-table *ucons-leaf-table*))
(defvar *ucons-leaf-table* (make-hash-table :test #'eq)
  "The table of all uconses whose cdr is NIL.")

(declaim (inline ucons)
         (notinline ucons--slow)
         (ftype (function (ucar ulist) ucons) ucons)
         (ftype (function (ucar ulist) ucons) ucons--slow))
(defun ucons (car cdr)
  "Given a suitable CAR and CDR, return a UCONS that is EQ to all future
   and past invocation of this function with the same arguments."
  (declare (type (or null ucons) cdr)
           (type ucar car))
  (let ((alist (and cdr
                    (listp (ucons-table cdr))
                    (ucons-table cdr))))
    (the ucons
         (or
          (loop for cons of-type (cons ucar ulist) in alist
                do (when (eq (car cons) car)
                     (return (cdr cons))))
          (ucons--slow car cdr)))))

(defun ucons--slow (car cdr)
  "Helper function of UCONS. Invoked when the UCONS-TABLE of CDR is not a
   list, or is a list but does not contain an entry for CAR."
  (declare (type (or ucons null) cdr)
           (type ucar car))
  (if (null cdr)
      (values (ensure-gethash car *ucons-leaf-table* (make-fresh-ucons car cdr)))
      (let ((table (ucons-table cdr)))
        (etypecase table
          (hash-table
           (values (ensure-gethash car table (make-fresh-ucons car cdr))))
          (list
           (let ((ucons (make-fresh-ucons car cdr)))
             (prog1 ucons
               (cond
                 ((> (length table) 8)
                  (setf (ucons-table cdr)
                        (alist-hash-table table :test #'eql :size 16))
                  (setf (gethash car (ucons-table cdr)) ucons))
                 (t
                  (push (cons car ucons) (ucons-table cdr)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ulist creation

(defun ulist (&rest args)
  "Return the ulist associated with the supplied arguments."
  (declare (dynamic-extent args))
  (labels ((%ulist (first rest)
             (if (null rest)
                 (ucons first nil)
                 (ucons first (%ulist (car rest) (cdr rest))))))
    (unless (null args)
      (%ulist (first args) (rest args)))))

(define-compiler-macro ulist (&whole whole &rest arg-forms)
  (if (> (length arg-forms) 9)
      whole
      (let ((gensyms
              (loop for arg-form in arg-forms
                    collect (gensym "ARG"))))
        `(let* ,(mapcar #'list gensyms arg-forms)
           ,(let (result-form)
              (loop for gensym in (reverse gensyms)
                    do (setf result-form `(ucons ,gensym ,result-form)))
              result-form)))))

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

(define-compiler-macro ulist* (&whole whole &rest arg-forms)
  (if (> (length arg-forms) 9)
      whole
      (let ((gensyms
              (loop for arg-form in arg-forms
                    collect (gensym "ARG"))))
        `(let* ,(mapcar #'list gensyms arg-forms)
           ,(let* ((rgensyms (reverse gensyms))
                   (result-form
                     `(prog1 ,(car rgensyms)
                        (check-type ,(car rgensyms)
                                    (or ucons null)))))
              (loop for gensym in (cdr rgensyms)
                    do (setf result-form `(ucons ,gensym ,result-form)))
              result-form)))))

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

(defmacro-driver (FOR var IN-ULIST u &optional BY (step ''ucdr))
  "All the elements of a ulist."
  (with-gensyms (ulist)
    `(progn
       (with ,ulist = ,u)
       (,(if generate 'generate 'for)
        ,var next
        (progn
          (if (not ,ulist)
              (terminate)
              (prog1 (ucar ,ulist)
                (setf ,ulist
                      (funcall-form ,step ,ulist)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; structured ulists

(defvar *ustruct-lambda-lists* (make-hash-table :test #'eq))

(defun parse-ustruct-lamba-list (ustruct-lambda-list)
  (let ((variadic? (find '&rest ustruct-lambda-list))
        slot-names slot-types)
    (dolist (element (remove '&rest ustruct-lambda-list))
      (etypecase element
        (symbol
         (push element slot-names)
         (push 'ucar   slot-types))
        (cons
         (assert (= 2 (length element)))
         (assert (symbolp (car element)))
         (push (first element) slot-names)
         (push (second element) slot-types))))
    (values (nreverse slot-names)
            (nreverse slot-types)
            variadic?)))

(defmacro define-ustruct (name &body ustruct-lambda-list)
  (multiple-value-bind (slot-names slot-types variadic?)
      (parse-ustruct-lamba-list ustruct-lambda-list)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *ustruct-lambda-lists*)
               ',ustruct-lambda-list))
       (declaim (inline ,name))
       (defun ,name ,slot-names
         (declare ,@(loop for slot-name in slot-names
                          for slot-type in slot-types
                          collect `(type ,slot-type ,slot-name)))
         (,(if variadic? 'ulist* 'ulist) ',name ,@slot-names)))))

(defmacro with-ustruct-accessors
    ((ustruct-name &key (prefix "") (suffix "")) ustruct &body body)
  (check-type ustruct-name symbol)
  (check-type prefix string-designator)
  (check-type suffix string-designator)
  (once-only (ustruct)
    (multiple-value-bind (ustruct-lambda-list ustruct-known?)
        (gethash ustruct-name *ustruct-lambda-lists*)
      (unless ustruct-known?
        (error "Not a ustruct name: ~A" ustruct-name))
      (multiple-value-bind (slot-names slot-types variadic?)
          (parse-ustruct-lamba-list ustruct-lambda-list)
        (let* ((extended-names
                 (loop for slot-name in slot-names
                       collect (symbolicate prefix slot-name suffix)))
               (typedecls (loop for extended-name in extended-names
                                for slot-type in slot-types
                                collect `(type ,slot-type ,extended-name))))
          `(progn
             (assert (eq (ucar ,ustruct) ',ustruct-name))
             (let ((,ustruct (ucdr ,ustruct)))
               ,(if variadic?
                    `(let* ,(loop for extended-name in (butlast extended-names)
                                  collect `(,extended-name (ucar ,ustruct))
                                  collect `(,ustruct (ucdr ,ustruct)))
                       (declare (ignorable ,ustruct ,@(butlast extended-names)))
                       (let ((,(last-elt extended-names) ,ustruct))
                         (declare (ignorable ,(last-elt extended-names))
                                  ,@typedecls)
                         ,@body))
                    `(let* ,(loop for extended-name in extended-names
                                  collect `(,extended-name (ucar ,ustruct))
                                  collect `(,ustruct (ucdr ,ustruct)))
                       (declare (ignorable ,ustruct ,@extended-names) ,@typedecls)
                       ,@body)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; other ulist utilities

(defun ulength (ulist)
  "Return the length of the given ulist."
  (declare (ulist ulist) (optimize speed))
  (iterate (for elt in-ulist ulist)
           (counting t)))

(defun ulist-shallow-copy (ulist)
  "Return a list of the elements of ULIST."
  (declare (ulist ulist))
  (loop while ulist
        collect (ucons-car ulist)
        do (setf ulist (ucons-cdr ulist))))

(defun ulist-deep-copy (ulist)
  "Return a tree of the same shape as ULIST, but where all occuring ulists
   have been converted to lists."
  (declare (ulist ulist))
  (loop while ulist
        collect (let ((car (ucons-car ulist)))
                  (if (uconsp car)
                      (ulist-deep-copy car)
                      car))
        do (setf ulist (ucons-cdr ulist))))

(defmethod print-object ((ulist ucons) stream)
  (cond (*print-pretty*
         (let ((list (ulist-shallow-copy ulist)))
           (pprint-logical-block (stream list :prefix "[" :suffix "]")
             (pprint-fill stream list nil))))
        (t
         (write-string "[" stream)
         (loop while ulist do
           (write (ucons-car ulist) :stream stream)
           (when (ucons-cdr ulist)
             (write-string " " stream))
           (setf ulist (ucons-cdr ulist)))
         (write-string "]" stream))))
