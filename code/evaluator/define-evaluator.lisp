;;; © 2016-2017 Marco Heisig - licensed under AGPLv3, see the file COPYING

(in-package :petalisp)

(defmacro define-evaluator (evaluator-name &rest instructions)
  "Given a symbol EVALUATOR-NAME and zero or more INSTRUCTIONS of the form
  (INSTRUCTION-NAME ((NAME-1 TYPE-1) ...) &BODY BODY), defines

  - A set of instructions named EVALUATOR-NAME-INSTRUCTION-NAME
  - A thread-safe queue to transmit instructions
  - A function to process the events sequentially, by dispatching on the
    type of each instruction and executing the corresponding BODY

  Finally, a new thread is created, invoking the previously defined
  processing function."
  (let ((queue (symbolicate "*" evaluator-name "-INSTRUCTIONS*"))
        (thread (symbolicate "*" evaluator-name "-THREAD*"))
        (thread-name (format nil "Petalisp ~A thread" evaluator-name))
        (instructions
          (iterate (for (name . rest) in instructions)
                   (collect `(,(symbolicate evaluator-name "-" name) ,@rest))))
        (current-instruction (gensym "INSTRUCTION")))
    `(progn
       (defvar ,queue (make-queue))
       ,@(iterate (for (instruction-name instruction-arguments . body) in instructions)
                  (for constructor-name = (symbolicate "MAKE-" instruction-name))
                  (for arguments = (mapcar #'first instruction-arguments))
                  (collect `(defstruct (,instruction-name
                                        (:constructor ,constructor-name ,arguments)
                                        (:copier nil)
                                        (:predicate nil))
                              ,@(iterate (for (name type) in instruction-arguments)
                                         (collect `(,name nil :type ,type)))))
                  (collect
                      `(defun ,instruction-name ,arguments
                         (enqueue ,queue (,constructor-name ,@arguments)))))
       (defun ,evaluator-name ()
         (let ((,current-instruction (dequeue ,queue)))
           (etypecase ,current-instruction
             ,@(iterate
                 (for (instruction-name instruction-arguments . body) in instructions)
                 (collect
                     `(,instruction-name
                       (let ,(iterate
                               (for (argument-name argument-type) in instruction-arguments)
                               (collect `(,argument-name
                                          (,(symbolicate instruction-name "-" argument-name)
                                           ,current-instruction))))
                         ,@body)))))))
       (defvar ,thread
         (make-thread #'(lambda () (declare (notinline ,evaluator-name))
                          (loop (,evaluator-name)))
                      :name ,thread-name)))))

