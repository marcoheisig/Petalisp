;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions.

(defgeneric device-name (device))

(defgeneric device-memory (device))

(defgeneric device-cores (device))

(defgeneric core-name (device))

(defgeneric core-memory (device))

(defgeneric memory-name (memory))

(defgeneric memory-parent (memory))

(defgeneric memory-children (memory))

(defgeneric memory-cores (memory))

(defgeneric memory-size (memory))

(defgeneric memory-granularity (memory))

(defgeneric memory-latency (memory))

(defgeneric memory-bandwidth (memory))

(defgeneric memory-parent-bandwidth (memory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes.

(defclass device ()
  (;; The name of the device, e.g., "AMD FX(tm)-8150"
   (%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :reader device-name
    :type string)
   ;; The memory hierarchy accessed by that device.
   (%memory
    :initarg :memory
    :initform (alexandria:required-argument :memory)
    :reader device-memory
    :type memory)
   (%number-of-cores
    :initarg :number-of-cores
    :reader device-number-of-cores
    :type (and (integer 1) fixnum))))

(defclass core ()
  ((%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :reader core-name
    :type string)
   (%memory
    :initarg :memory
    :initform (alexandria:required-argument :memory)
    :reader core-memory
    :type memory)))

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
   ;; The cores with access to this piece of memory.
   (%cores
    :initarg :cores
    :initform '()
    :reader memory-cores
    :accessor memory-cores-slot
    :type list)
   ;; The memory size in bytes.
   (%size
    :initarg :size
    :initform (alexandria:required-argument :size)
    :reader memory-size
    :type unsigned-byte)
   ;; The memory granularity, i.e., the minimum number of elements
   ;; transferred per load or store operation in bytes.
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
   ;; The memory bandwidth for communicating with the parent.
   (%parent-bandwidth
    :initarg :parent-bandwidth
    :initform nil
    :reader memory-parent-bandwidth
    :type (or null unsigned-byte))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod shared-initialize :after ((memory memory) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (unless (null (memory-parent memory))
    (pushnew memory (memory-children-slot (memory-parent memory)))))

(defmethod shared-initialize :after ((core core) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (labels ((register-core-with-memory (memory)
             (unless (null memory)
               (pushnew core (memory-cores-slot memory))
               (register-core-with-memory (memory-parent memory)))))
    (register-core-with-memory (core-memory core))))

(defmethod print-object ((device device) stream)
  (format stream "~@<#<~;~S ~_~@{~S ~:_~S~^ ~_~}~;>~:>"
          (class-name (class-of device))
          :name (device-name device)
          :memory (device-memory device)
          :cores (device-cores device)))

(defmethod print-object ((core core) stream)
  (format stream "~@<#<~;~S ~_~@{~S ~:_~S~^ ~_~}~;>~:>"
          (class-name (class-of core))
          :name (core-name core)
          :memory (core-memory core)))

(defmethod print-object ((memory memory) stream)
  (format stream "~@<#<~;~S ~_~@{~S ~:_~S~^ ~_~}~;>~:>"
          (class-name (class-of memory))
          :name (memory-name memory)
          :size (memory-size memory)
          :granularity (memory-granularity memory)
          :latency (memory-latency memory)
          :bandwidth (memory-bandwidth memory)
          :parent (memory-parent memory)
          :parent-bandwidth (memory-parent-bandwidth memory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Host Device

;;; Use a bunch of heuristics to figure out the machine model of the host.
(defun host-device ()
  #+linux
  (linux-host-device)
  #-(or linux)
  (fallback-host-device))

;;; Return the machine model for a 'typical' machine with a give number of
;;; cores.  This is a fallback solution - all numbers have been made up :)
(defun fallback-host-device ()
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
             :parent-bandwidth 8))
         (cores
           (loop for id below n-cpus
                 collect
                 (make-instance 'core
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
                       :parent-bandwidth 16))))))
    (make-instance 'device
      :name (or (machine-type) "fallback-host-device")
      :cores cores
      :main-memory ram)))

(defun linux-host-device ()
  (let* ((ram (make-instance 'memory
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
                :latency 400))
         (online (uiop:ensure-pathname "/sys/devices/system/cpu/online"
                                       :want-existing t))
         (core-ids (parse-integer-set (alexandria:read-file-into-string online)))
         ;; Records of the form (id level cache).
         (caches '()))
    (make-instance 'device
      :main-memory ram
      :cores
      (labels ((cache-level (cache-dir)
                 (parse-integer
                  (alexandria:read-file-into-string
                   (merge-pathnames cache-dir "level"))))
               (cache-id (cache-dir)
                 (parse-integer
                  (alexandria:read-file-into-string
                   (merge-pathnames cache-dir "id"))))
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
                 (let* ((id (cache-id cache-dir))
                        (level (cache-level cache-dir))
                        (entry (find-if (lambda (entry)
                                          (and (= id (first entry))
                                               (= level (second entry))))
                                        caches)))
                   (if (not entry)
                       (let ((cache (make-cache cache-dir level)))
                         (push (list id level cache) caches)
                         cache)
                       (third entry)))))
        (loop for core-id in core-ids
              collect
              (let* ((cpu-dir
                       (uiop:ensure-pathname
                        (format nil "/sys/devices/system/cpu/cpu~D/" core-id)
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
                (make-instance 'core
                  :name (first (last (pathname-directory cpu-dir)))
                  :memory (ensure-cache
                           (find-if-not
                            (lambda (cache-dir)
                              (eq (cache-type cache-dir) :instruction))
                            (reverse cache-dirs))))))))))

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

