;;;; © 2016-2020 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions.

;;; The name of the memory, e.g., "L1" or "RAM".
(defgeneric memory-name (memory))

;;; Returns the parent memory.
(defgeneric memory-parent (memory))

;;; Returns the list memories that have this one as their parent.
(defgeneric memory-children (memory))

;;; Returns the memory size in bytes.
(defgeneric memory-size (memory))

;;; Returns the granularity of the memory, i.e., how many elements are
;;; transferred at minimum for a single access.
(defgeneric memory-granularity (memory))

;;; Returns the memory latency in cycles.
(defgeneric memory-latency (memory))

;;; Returns the memory bandwidth in bytes per cycle for accessing elements
;;; in this memory hierarchy.
(defgeneric memory-bandwidth (memory))

;;; Returns the bandwidth for communicating data with the parent.
(defgeneric memory-parent-bandwidth (memory))

;;; Returns the name of the processor.
(defgeneric processor-name (processor))

;;; Returns the memory of the processor.
(defgeneric processor-memory (processor))

;;; Returns the name of a machine.
(defgeneric machine-name (machine))

;;; Returns the processors of a machine.
(defgeneric machine-processors (machine))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes.

(defclass memory ()
  (;; The name of the memory, e.g., "L1" or "RAM"
   (%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :reader memory-name
    :type string)
   ;; Most modern machines have multiple kinds of memory, arranged in a
   ;; hierarchical fashion.  The parent slot contains the memory model of
   ;; the next higher (bigger, slower) level of memory, or null, if this is
   ;; the topmost level of memory.
   (%parent
    :initarg :parent
    :initform nil
    :reader memory-parent
    :type (or null memory))
   ;; The children slot contains a list of memory descriptions of each
   ;; lower (smaller, faster) level of memory.
   (%children
    :initarg :children
    :initform '()
    :reader memory-children
    :accessor memory-children-slot
    :type list)
   ;; The memory size in bytes.
   (%size
    :initarg :size
    :initform (alexandria:required-argument :size)
    :reader memory-size
    :type unsigned-byte)
   ;; The memory granularity in bytes.
   (%granularity
    :initarg :granularity
    :initform (alexandria:required-argument :granularity)
    :reader memory-granularity
    :type unsigned-byte)
   ;; The memory latency in cycles.
   (%latency
    :initarg :latency
    :initform (alexandria:required-argument :latency)
    :reader memory-latency
    :type unsigned-byte)
   ;; The memory bandwidth, in bytes per cycle.
   (%bandwidth
    :initarg :bandwidth
    :initform (alexandria:required-argument :bandwidth)
    :reader memory-bandwidth
    :type unsigned-byte)
   (%parent-bandwidth
    :initarg :parent-bandwidth
    :initform nil
    :reader memory-parent-bandwidth
    :type (or null unsigned-byte))))

(defclass processor ()
  (;; The name of the processor, e.g., "AMD FX(tm)-8150 Core #2"
   (%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :reader processor-name
    :type string)
   ;; The memory hierarchy accessed by that processor.
   (%memory
    :initarg :memory
    :initform (alexandria:required-argument :memory)
    :reader processor-memory
    :type memory)))

(defclass machine ()
  (;; The name of the machine, e.g., "alice-laptop"
   (%name
    :initarg :name
    :initform (or (machine-instance) "Host")
    :reader machine-name
    :type string)
   ;; The processors of that machine.
   (%processors
    :initarg :processors
    :reader machine-processors
    :type list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod shared-initialize :after ((memory memory) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (unless (null (memory-parent memory))
    (pushnew memory (memory-children-slot (memory-parent memory)))))

(defmethod print-object ((memory memory) stream)
  (format stream "~@<#<~;~S ~_~@{~S ~:_~S~^ ~_~}~;#>~:>"
          (class-name (class-of memory))
          :name (memory-name memory)
          :size (memory-size memory)
          :granularity (memory-granularity memory)
          :latency (memory-latency memory)
          :bandwidth (memory-bandwidth memory)
          :parent (memory-parent memory)
          :parent-bandwidth (memory-parent-bandwidth memory)))

(defmethod print-object ((processor processor) stream)
  (format stream "~@<#<~;~S ~_~@{~S ~:_~S~^ ~_~}~;#>~:>"
          (class-name (class-of processor))
          :name (processor-name processor)
          :memory (processor-memory processor)))

(defmethod print-object ((machine machine) stream)
  (format stream "~@<#<~;~S ~_~@{~S ~:_~S~^ ~_~}~;#>~:>"
          (class-name (class-of machine))
          :name (machine-name machine)
          :processors (machine-processors machine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initialization

;;; Use a bunch of heuristics to figure out the machine model of the host.
(defun host-machine ()
  #+linux
  (linux-host-machine)
  #-(or linux)
  (default-host-machine))

(defun linux-host-machine ()
  (make-instance 'machine
    :processors
    (let* ((online (uiop:ensure-pathname "/sys/devices/system/cpu/online" :want-existing t))
           (cpus (parse-integer-set (alexandria:read-file-into-string online)))
           ;; Records of the form (id level cache).
           (caches '())
           (ram
             (make-instance 'memory
               :name "RAM"
               :size
               (with-open-file (stream "/proc/meminfo")
                 (loop for line = (read-line stream nil nil)
                       while line
                       do (let ((position (mismatch #1="MemTotal:" line)))
                            (when (= position (length #1#))
                              (return
                                (parse-size line :start position))))))
               ;; TODO These values are just educated guesses.
               :granularity 64
               :throughput 64
               :bandwidth 64
               :latency 400)))
      (labels ((cache-level (cache-dir)
                 (parse-integer
                  (alexandria:read-file-into-string
                   (merge-pathnames cache-dir "level"))))
               (cache-type (cache-dir)
                 (parse-cache-type
                  (alexandria:read-file-into-string
                   (merge-pathnames cache-dir "type"))))
               (make-cache (cache-dir level)
                 (make-instance 'memory
                   :name (format nil "L~D" (cache-level cache-dir))
                   :size
                   (parse-size
                    (alexandria:read-file-into-string
                     (merge-pathnames cache-dir "size")))
                   :granularity
                   (parse-size
                    (alexandria:read-file-into-string
                     (merge-pathnames cache-dir "coherency_line_size")))
                   :parent
                   (or (third (find (1+ level) caches :key #'second))
                       ram)
                   ;; TODO These values are just educated guesses.
                   :bandwidth 64
                   :parent-bandwidth 64
                   :latency (* level level 5)))
               (ensure-cache (cache-dir)
                 (let* ((id (parse-integer
                             (alexandria:read-file-into-string
                              (merge-pathnames cache-dir "id"))))
                        (entry (find id caches :key #'first)))
                   (if (not entry)
                       (let* ((level (cache-level cache-dir))
                              (cache (make-cache cache-dir level)))
                         (push (list id level cache) caches)
                         cache)
                       (third entry)))))
        (loop for cpu in cpus
              collect
              (let* ((cpu-dir
                       (uiop:ensure-pathname
                        (format nil "/sys/devices/system/cpu/cpu~D/" cpu)
                        :want-existing t
                        :want-directory t))
                     (cache-dirs
                       ;; Sort the cache directories such that parent caches
                       ;; are registered first.
                       (sort
                        (remove-if-not
                         (lambda (dir)
                           (uiop:file-exists-p (merge-pathnames dir "level")))
                         (uiop:subdirectories
                          (make-pathname
                           :directory
                           (append (pathname-directory cpu-dir)
                                   (list "cache")))))
                        #'>
                        :key #'cache-level)))
                (mapc #'ensure-cache cache-dirs)
                (make-instance 'processor
                  :name (first (last (pathname-directory cpu-dir)))
                  :memory (ensure-cache
                           (find-if
                            (lambda (cache-dir)
                              (not (eq (cache-type cache-dir) :instruction)))
                            cache-dirs
                            :from-end t)))))))))

(defparameter *suffix-factors*
  (let ((table (make-hash-table :test #'equalp))
        (entries
          `((,(expt 1024 1) "k" "kb" "kib" "kilobyte")
            (,(expt 1024 2) "m" "mb" "mib" "megabyte")
            (,(expt 1024 3) "g" "gb" "gib" "gigabyte")
            (,(expt 1024 4) "t" "tb" "tib" "terrabyte")
            (,(expt 1024 5) "p" "pb" "pib" "petapyte")
            (,(expt 1024 6) "e" "eb" "eib" "exabyte")
            (,(expt 1024 7) "z" "zb" "zib" "zettabyte"))))
    (loop for (factor . suffixes) in entries do
      (loop for suffix in suffixes do
        (setf (gethash suffix table)
              factor)))
    table))

(defun suffix-factor (suffix)
  (or (gethash suffix *suffix-factors*)
      (error "Unknown suffix: ~S" suffix)))

(defun parse-size (string &key (start 0) end)
  (declare (string string))
  (let* ((end (or end (length string)))
         (suffix (position-if #'alpha-char-p string
                              :start start
                              :end end
                              :from-end t)))
    (if (not suffix)
        (parse-integer string :start start :end end)
        (let* ((suffix-start (1+ (position-if-not #'alpha-char-p string
                                                  :start start
                                                  :end suffix
                                                  :from-end t)))
               (size (parse-integer string :start start :end suffix-start)))
          (* size (suffix-factor (subseq string suffix-start (1+ suffix))))))))

(defun parse-cache-type (string)
  (setf string (subseq string 0 (position-if-not #'alpha-char-p string)))
  (cond ((string-equal string "Data") :data)
        ((string-equal string "Instruction") :instruction)
        ((string-equal string "Unified") :unified)
        (t (error "Unknown cache type: ~S" string))))

;;; This is essentially a pure CL version of the Linux kernel's
;;; cpulist_parse() function.
(defun parse-integer-set (string)
  (declare (string string))
  (let ((ranges (split-sequence:split-sequence #\, string)))
    (petalisp.utilities:with-collectors ((integers collect))
      (loop for range in ranges do
        (let* ((hyphen (position #\- range))
               (colon (position #\: range :start (or hyphen 0)))
               (slash (position #\/ range :start (or colon 0)))
               (lo (parse-integer range :end hyphen))
               (hi
                 (if (null hyphen)
                     lo
                     (parse-integer range :start (1+ hyphen) :end colon)))
               (used-size
                 (if (null colon)
                     1
                     (parse-integer range :start (1+ colon) :end slash)))
               (group-size
                 (if (null slash)
                     1
                     (parse-integer range :start (1+ slash)))))
          (loop for base from lo to hi by group-size do
            (loop for offset below used-size do
              (when (<= (+ base offset) hi)
                (collect (+ base offset)))))))
      (remove-duplicates (integers)))))

;;; Return the machine model for a 'typical' machine with a give number of
;;; cores.  This is a fallback solution - all numbers have been made up :)
(defun default-host-machine ()
  (let* ((n-cpus (petalisp.utilities:number-of-cpus))
         (ram
           ;; We assume two GB of RAM per CPU.
           (make-instance 'memory
             :name "RAM"
             :size (* n-cpus 2 1024 1024 1024)
             :granularity 64
             :latency 400
             :bandwidth (* n-cpus 4)))
         (l3-cache
           ;; We assume two MB of L3 cache per CPU, shared among all CPUs.
           (make-instance 'memory
             :name "L3"
             :parent ram
             :size (* n-cpus 2 1024 1024)
             :granularity 64
             :latency 40
             :bandwidth 64
             :parent-bandwidth 8)))
    (make-instance 'machine
      :processors
      (loop for id below n-cpus
            collect
            (make-instance 'processor
              :name (format nil "CPU-~D" id)
              :memory
              (make-instance 'memory
                :name "L1"
                ;; We assume  32 KB of L1 cache per CPU.
                :size (* 32 1024)
                :granularity 64
                :latency 5
                :bandwidth 128
                :parent-bandwidth 32
                :parent
                (make-instance 'memory
                  :name "L2"
                  :parent l3-cache
                  ;; We assume  512 KB of L2 cache per CPU.
                  :size (* n-cpus 512 1024)
                  :granularity 64
                  :latency 15
                  :bandwidth 64
                  :parent-bandwidth 16)))))))
