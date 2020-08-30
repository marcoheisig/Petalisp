;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;; The following criteria are used to select the strided arrays that are
;;; registered in the layout table:
;;;
;;; 1. The node is one of the root nodes.
;;;
;;; 2. The node is referenced by multiple other nodes.
;;;
;;; 3. The node is the target of a broadcasting reference.
;;;
;;; 4. The node is an immediate node.
;;;
;;; These rules ensure that values that are used more than once are only
;;; computed once.
;;;
;;; The derivation of the layout table of a graph consists of two steps.
;;; In the first step, we build a hash table that assigns some strided
;;; arrays the values :POTENTIALLY-SPECIAL or :SPECIAL.  In the second
;;; step, this table turned into a layout table by replacing :SPECIAL
;;; values by buffers and by discarding all other hash table entries.

(defmacro layout-table-entry (node)
  `(values (gethash ,node *layout-table*)))

(defmacro with-layout-table ((graph-roots) &body body)
  `(call-with-layout-table
    ,graph-roots
    (lambda () ,@body)))

(defun call-with-layout-table (graph-roots thunk)
  (let ((*layout-table* (make-hash-table :test #'eq)))
    (loop for graph-root in graph-roots do
      ;; Rule 1
      (traverse-node graph-root t))
    ;; At this point, the symbol table maps from lazy arrays to the
    ;; keywords :special and :potentially-special.  In the next step, we
    ;; replace the :special entries with suitable layouts and discard all
    ;; other entries.
    ;;
    ;; We start by converting the root nodes, because those are even more
    ;; special - root nodes are always assigned array-immediate-layouts,
    ;; because we want to pass their content to Lisp later on.
    (loop for graph-root in graph-roots
          unless (typep graph-root 'array-immediate)
            do (setf (gethash graph-root *layout-table*)
                     (make-array-immediate-layout
                      graph-root
                      (make-array (shape-dimensions (array-shape graph-root))
                                  :element-type (element-type graph-root)))))
    ;; Now we convert all other entries.
    (maphash
     (lambda (lazy-array value)
       (when (symbolp value)            ; Skip root nodes.
         (if (eq value :special)
             (setf (layout-table-entry lazy-array)
                   (make-layout lazy-array))
             (remhash lazy-array *layout-table*))))
     *layout-table*)
    ;; Invoke the supplied thunk.
    (funcall thunk)))

(defun traverse-node (node special-p)
  (multiple-value-bind (traverse-inputs-p inputs-special-p)
      (visit-node node)
    (when special-p
      (setf (layout-table-entry node) :special))
    (when traverse-inputs-p
      (mapc
       (lambda (input)
         (traverse-node input inputs-special-p))
       (inputs node)))))

(defgeneric visit-node (node))

(defmethod visit-node ((node lazy-array))
  ;; Rule 2.
  (case (lazy-array-refcount node)
    ((0 1) (values t nil))
    (otherwise
     (case (layout-table-entry node)
       ((nil)
        (setf (layout-table-entry node) :potentially-special)
        (values t nil))
       ((:potentially-special)
        (setf (layout-table-entry node) :special)
        (values nil nil))
       ((:special)
        (values nil nil))))))

(defmethod visit-node ((immediate immediate))
  (setf (layout-table-entry immediate) :special)
  (values nil nil))

(defmethod visit-node ((lazy-reshape lazy-reshape))
  (multiple-value-bind (traverse-inputs-p inputs-special-p)
      (call-next-method)
    (if (not traverse-inputs-p)
        (values nil nil)
        (let ((transformation (transformation lazy-reshape)))
          (values
           traverse-inputs-p
           ;; Rule 3.
           (or inputs-special-p (not (transformation-invertiblep transformation))))))))
