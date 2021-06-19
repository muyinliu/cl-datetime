(in-package :dt)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

;; struct timeval {
;;     time_t      tv_sec;     /* seconds */
;;     suseconds_t tv_usec;    /* microseconds */
;; };
(cffi:defcstruct timeval
  (tv-sec :unsigned-long)
  (tv-usec :unsigned-long))

;; int gettimeofday(struct timeval *tv, struct timezone *tz);
(declaim (inline %gettimeofday))
(cffi:defcfun ("gettimeofday" %gettimeofday) :int
  (timeval (:pointer (:struct timeval)))
  (timezone :pointer))

(declaim (inline gettimeofday))
(defun gettimeofday ()
  "gettimeofday can only accurate to microsecond(us)."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cffi:with-foreign-object (timeval '(:struct timeval))
    (%gettimeofday timeval (cffi:null-pointer))
    (cffi:with-foreign-slots ((tv-sec tv-usec) timeval (:struct timeval))
      (declare (type (unsigned-byte 64) tv-sec tv-usec))
      (values tv-sec tv-usec))))

(declaim (inline get-unix-time-us))
(defun get-unix-time-us ()
  "get unix-time in microseconds(us)."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (tv-sec tv-usec)
      (gettimeofday)
    (declare (type (unsigned-byte 64) tv-sec tv-usec))
    (+ (* tv-sec 1000000)
       tv-usec)))

(declaim (inline get-unix-time-ms))
(defun get-unix-time-ms ()
  "get unix-time in millisecond(ms)."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (tv-sec tv-usec)
      (gettimeofday)
    (declare (type integer tv-sec tv-usec))
    (+ (* tv-sec 1000)
       (truncate tv-usec 1000))))
