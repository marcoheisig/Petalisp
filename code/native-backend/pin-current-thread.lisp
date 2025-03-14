(in-package #:petalisp.native-backend)

(defparameter *pin-current-thread-code*
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
}}")

(defparameter *pin-current-thread-library*
  (ignore-errors
   (load-foreign-code *pin-current-thread-code*)))

(defun pin-current-thread (cpu-number)
  "Ensure that the current thread will only run on the specified CPU.  Returns
whether the pinning was successful."
  (declare (type unsigned-byte cpu-number))
  (when *pin-current-thread-library*
    (zerop
     (cffi:foreign-funcall "petalisp_native_backend_pin_current_thread" :int cpu-number :int))))
