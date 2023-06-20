;;;; © 2016-2023 Marco Heisig         - license: GNU AGPLv3 -*- coding: utf-8 -*-

(in-package #:petalisp.native-backend)

#+(or windows)
(defun pin-current-thread (cpu-number)
  (declare (ignore cpu-number))
  (values))

#-(or windows)
(let* ((code
         "
#include <errno.h>
#include <sched.h>
#include <pthread.h>
#include <unistd.h>

extern \"C\" {
int petalisp_native_backend_pin_current_thread(int core_id) {
    int num_cores = sysconf(_SC_NPROCESSORS_ONLN);
    if (core_id < 0 || core_id >= num_cores) return EINVAL;

    cpu_set_t cpuset;
    CPU_ZERO(&cpuset);
    CPU_SET(core_id, &cpuset);

    pthread_t current_thread = pthread_self();
    return pthread_setaffinity_np(current_thread, sizeof(cpu_set_t), &cpuset);
}
}

         "))
  (load-foreign-code code)
  (defun pin-current-thread (cpu-number)
    (cffi:foreign-funcall "petalisp_native_backend_pin_current_thread" :int cpu-number)))
