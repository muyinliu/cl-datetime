(in-package :dt)


;; struct timeval {
;;     time_t      tv_sec;     /* seconds */
;;     suseconds_t tv_usec;    /* microseconds */
;; };
(cffi:defcstruct timeval
  (tv-sec :unsigned-long)
  (tv-usec :unsigned-long))

;; int gettimeofday(struct timeval *tv, struct timezone *tz);
(cffi:defcfun ("gettimeofday" %gettimeofday) :int
  (timeval (:pointer (:struct timeval)))
  (timezone :pointer))

(defun gettimeofday ()
  "gettimeofday can only accurate to microsecond(us)."
  (cffi:with-foreign-object (timeval '(:struct timeval))
    (%gettimeofday timeval (cffi:null-pointer))
    (cffi:with-foreign-slots ((tv-sec tv-usec) timeval (:struct timeval))
      (values tv-sec tv-usec))))

(defun get-unix-time-us ()
  "get unix-time in microseconds(us)."
  (multiple-value-bind (tv-sec tv-usec)
      (gettimeofday)
    (declare (type integer tv-sec tv-usec))
    (+ (* tv-sec 1000000)
       tv-usec)))

(defun get-unix-time-ms ()
  "get unix-time in millisecond(ms)."
  (multiple-value-bind (tv-sec tv-usec)
      (gettimeofday)
    (declare (type integer tv-sec tv-usec))
    (+ (* tv-sec 1000)
       (truncate tv-usec 1000))))
