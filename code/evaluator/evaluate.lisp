;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defvar *kernel-table* (make-hash-table :test #'eq :weakness :key))

(defmethod enqueue (&rest objects)
  ;; with Petalisp mutation lock
  (let ((use-table (make-hash-table :test #'eq)))
    (labels ((populate-use-table (node)
               (dolist (input (inputs node))
                 (let ((users (gethash input use-table)))
                   (unless (member node users)
                     (if users
                         (push node (gethash input use-table))
                         (setf (gethash input use-table) (list node)))
                     (mapc #'populate-use-table (inputs node)))))))
      (mapcar #'populate-use-table objects))
    (labels ((dependencies (node)
               (let (dependencies)
                 (labels ((recurse (node)
                            (if (elaboration? node)
                                (push node dependencies)
                                (mapc #'dependencies (inputs node)))))
                   (recurse node))))
             (recurse (node)
               (unless (elaboration? node)
                 (if (or (< 1 (length (gethash node use-table)))
                         (member node objects :test #'eq))
                     (let ((recipe (shallow-copy node)))
                       (mapc #'recurse (inputs recipe))
                       (change-class node 'strided-array-elaboration
                                     :dependencies (dependencies recipe)
                                     :recipe recipe))
                     (mapc #'recurse (inputs node))))))
      (mapc #'recurse objects)))
  (apply #'values objects))

;(defmethod wat-for-completion (&rest objects))
