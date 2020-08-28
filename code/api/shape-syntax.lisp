;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

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
      '((~))
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

(declaim (list *shape-syntax*))
(defvar *shape-syntax* '())

(defstruct (shape-syntax
            (:copier nil)
            (:predicate shape-syntax-p))
  (delimiter nil :type symbol)
  (constructor-parser nil :type function)
  (pattern-parser nil :type function))

(defun find-shape-syntax-no-error (delimiter)
  (find delimiter *shape-syntax* :key #'shape-syntax-delimiter))

(defun find-shape-syntax (delimiter)
  (or (find-shape-syntax-no-error delimiter)
      (error "Unknown shape syntax delimiter: ~S" delimiter)))

(defun ensure-shape-syntax (delimiter constructor-parser pattern-parser)
  (let ((syntax (find-shape-syntax-no-error delimiter)))
    (when (shape-syntax-p syntax)
      (setf (shape-syntax-constructor-parser syntax)
            constructor-parser)
      (setf (shape-syntax-pattern-parser syntax)
            pattern-parser)
      (return-from ensure-shape-syntax syntax))
    (let ((syntax (make-shape-syntax
                   :delimiter delimiter
                   :constructor-parser constructor-parser
                   :pattern-parser pattern-parser)))
      (push syntax *shape-syntax*)
      syntax)))

(defun shape-syntax-delimiter-p (object)
  (and (find-shape-syntax-no-error object)
       t))

(deftype shape-syntax-delimiter ()
  '(and symbol (satisfies shape-syntax-delimiter-p)))

(trivia:defpattern non-~ ()
  '(type (not shape-syntax-delimiter)))

(defmacro define-shape-syntax
    (delimiter (tokens collect finish) constructor-syntax pattern-syntax)
  (check-type delimiter symbol)
  (alexandria:with-gensyms (collect-fn finish-fn)
    `(progn
       (defconstant ,delimiter ',delimiter)
       (ensure-shape-syntax
        ',delimiter
        (lambda (,tokens ,collect-fn ,finish-fn)
          (declare (list ,tokens))
          (declare (function ,collect-fn ,finish-fn))
          (flet ((,collect (range)
                   (funcall ,collect-fn range))
                 (,finish (finisher)
                   (funcall ,finish-fn finisher)))
            (declare (ignorable (function ,collect) (function ,finish)))
            ,constructor-syntax))
        (lambda (,tokens ,collect-fn ,finish-fn)
          (flet ((,collect (range)
                   (funcall ,collect-fn range))
                 (,finish (finisher)
                   (funcall ,finish-fn finisher)))
            (declare (ignorable (function ,collect) (function ,finish)))
            ,pattern-syntax)))
       (defun ,delimiter (&rest args)
         (parse-shape
          (list* ',delimiter args)))
       (trivia:defpattern ,delimiter (&rest args)
         (parse-shape-pattern
          (list* ',delimiter args))))))

(defun parse-shape (syntax)
  (petalisp.utilities:with-collectors ((ranges collect))
    (flet ((finish (finisher)
             (return-from parse-shape
               (funcall finisher (ranges)))))
      (loop until (null syntax)
            for delimiter = (car syntax)
            for parser = (shape-syntax-constructor-parser (find-shape-syntax delimiter))
            do (setf syntax (funcall parser (rest syntax) #'collect #'finish)))
      (make-shape (ranges)))))

(defun parse-shape-pattern (syntax)
  (petalisp.utilities:with-collectors ((patterns collect))
    (flet ((finish (finisher)
             (return-from parse-shape-pattern
               (alexandria:with-gensyms (it)
                 `(trivia:guard1 ,it (shapep ,it)
                                 (shape-ranges ,it)
                                 ,(funcall finisher (patterns)))))))
      (loop until (null syntax)
            for delimiter = (car syntax)
            for parser = (shape-syntax-pattern-parser (find-shape-syntax delimiter))
            do (setf syntax (funcall parser (rest syntax) #'collect #'finish)))
      (finish
       (lambda (patterns)
         `(list ,@patterns))))))

(define-shape-syntax ~s (tokens collect finish)
  (trivia:ematch tokens
    ((list* shape rest)
     (mapc #'collect (shape-ranges shape))
     rest))
  (trivia:ematch tokens
    ((list* shape rest)
     (unless (and (listp shape) (shape-syntax-delimiter-p (car shape)))
       (error "The token after a ~S pattern must be a valid shape pattern."
              ~s))
     (append shape rest))))

(define-shape-syntax ~l (tokens collect finish)
  (trivia:ematch tokens
    ((list* ranges rest)
     (mapc #'collect ranges)
     rest))
  (trivia:ematch tokens
    ((list* ranges rest)
     (unless (null rest)
       (error "~S must only appear at the last clause of a shape pattern."
              ~l))
     (finish
      (lambda (patterns)
        `(list* ,@patterns ,ranges))))))

(define-shape-syntax ~r (tokens collect finish)
  (trivia:ematch tokens
    ((list* range rest)
     (collect range)
     rest))
  (trivia:ematch tokens
    ((list* range rest)
     (collect `(and ,range (type range)))
     rest)))

(define-shape-syntax ~ (tokens collect finish)
  (trivia:ematch tokens
    ((list* (and start (non-~)) (and end (non-~)) (and step (non-~)) rest)
     (collect (range start end step))
     rest)
    ((list* (and start (non-~)) (and end (non-~)) rest)
     (collect (range start end))
     rest)
    ((list* (and n (non-~)) rest)
     (collect (range n))
     rest)
    ((list)
     (finish
      (lambda (ranges)
        (unless (null ranges)
          (error "The empty ~S pattern is only appear at the end of a shape pattern."
                 ~))
        (load-time-value
         (make-shape '()))))))
  (trivia:match tokens
    ((list* (and start (non-~)) (and end (non-~)) (and step (non-~)) rest)
     (collect `(range ,start ,end ,step))
     rest)
    ((list* (and start (non-~)) (and end (non-~)) rest)
     (collect `(range ,start ,end))
     rest)
    ((list* (and n (non-~)) rest)
     (collect `(range ,n))
     rest)))
