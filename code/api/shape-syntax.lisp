;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printing

(defmethod print-object ((shape shape) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "~:<~@{~:_~<~S~^ ~D~^ ~D~^ ~D~:>~^ ~}~:>"
              (listify-shape-for-printing shape))))

(defun listify-shape-for-printing (shape)
  (if (zerop (shape-rank shape))
      '((~*))
      (mapcar #'listify-range-for-printing
              (shape-ranges shape))))

(defun listify-range-for-printing (range)
  (if (empty-range-p range)
      `(~ 0)
      (with-accessors ((start range-start)
                       (end range-end)
                       (step range-step)) range
        (if (= step 1)
            (if (zerop start)
                `(~ ,end)
                `(~ ,start ,end))
            `(~ ,start ,end ,step)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constructors and Pattern Matching

(defconstant ~ '~)

(defconstant ~* '~*)

(deftype shape-syntax-delimiter ()
  '(member ~ ~*))

(trivia:defpattern non-~ ()
  '(type (not shape-syntax-delimiter)))

(defun ~ (&rest tokens)
  (parse-shape (list* '~ tokens)))

(defun ~* (&rest tokens)
  (parse-shape (list* '~* tokens)))

(defun parse-shape (syntax)
  (let ((ranges '()))
    (labels ((parse-recursively (rest)
               (trivia:match rest
                 ((list)
                  (values))
                 ((list* '~ (and start (non-~)) (and end (non-~)) (and step (non-~)) rest)
                  (push (range start end step) ranges)
                  (parse-recursively rest))
                 ((list* '~ (and start (non-~)) (and end (non-~)) rest)
                  (push (range start end) ranges)
                  (parse-recursively rest))
                 ((list* '~ (and size (non-~)) rest)
                  (push (range size) ranges)
                  (parse-recursively rest))
                 ((list* '~* rest)
                  (loop for subseq on rest for first = (first subseq) do
                    (typecase first
                      (shape-syntax-delimiter
                       (parse-recursively subseq)
                       (loop-finish))
                      (range
                       (push first ranges))
                      (list
                       (dolist (object first)
                         (unless (rangep object)
                           (error "~@<Not a range: ~S~:@>"
                                  object))
                         (push object ranges)))
                      (otherwise
                       (error "~@<Neither range nor list of ranges: ~S~:@>"
                              first)))))
                 (_
                  (error "~@<Invalid shape syntax: ~S~:@>" rest))))
             (gather-ranges (object)
               (typecase object
                 (range (push object ranges))
                 (list (mapc #'gather-ranges object))
                 (otherwise
                  ))))
      (parse-recursively syntax))
    (make-shape (nreverse ranges))))

(trivia:defpattern ~ (&rest args)
   (parse-shape-pattern `(~ ,@args)))

(trivia:defpattern ~* (&rest args)
  (parse-shape-pattern `(~* ,@args)))

(defun parse-shape-pattern (syntax)
  (let ((patterns '()))
    (labels ((parse-recursively (rest)
               (trivia:match rest
                 ((list)
                  (values))
                 ((list* '~ (and start (non-~)) (and end (non-~)) (and step (non-~)) rest)
                  (push `(range ,start ,end ,step) patterns)
                  (parse-recursively rest))
                 ((list* '~ (and start (non-~)) (and end (non-~)) rest)
                  (push `(range ,start ,end) patterns)
                  (parse-recursively rest))
                 ((list* '~ (and size (non-~)) rest)
                  (push `(range ,size) patterns)
                  (parse-recursively rest))
                 ((list* '~* rest)
                  (loop for subseq on rest for first = (first subseq) do
                    (cond ((typep first 'shape-syntax-delimiter)
                           (parse-recursively subseq)
                           (loop-finish))
                          ((and (listp first)
                                (eql (first first) 'list))
                           (dolist (pattern (rest first))
                             (push pattern patterns)))
                          (t
                           (push first patterns)))))
                 (_
                  (error "~@<Invalid shape pattern syntax: ~S~:@>" rest)))))
      (parse-recursively syntax))
    (alexandria:with-gensyms (it)
      `(trivia:guard1
        ,it
        (shapep ,it)
        (shape-ranges ,it)
        (list* ,@(reverse patterns) _)))))
