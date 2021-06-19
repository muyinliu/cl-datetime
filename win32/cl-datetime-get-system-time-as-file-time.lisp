(in-package :dt)


(cffi:define-foreign-library kernel32
  (:windows "C:/WINDOWS/system32/kernel32.dll"))

(cffi:use-foreign-library kernel32)

;; C++ Syntax from https://msdn.microsoft.com/en-us/library/windows/desktop/ms724284(v=vs.85).aspx
;; typedef struct _FILETIME {
;;   DWORD dwLowDateTime;
;;   DWORD dwHighDateTime;
;; } FILETIME, *PFILETIME;
(cffi:defcstruct FILETIME
  (dwLowDateTime  :unsigned-int)
  (dwHighDateTime :unsigned-int))

;; C++ Syntax from https://msdn.microsoft.com/en-us/library/windows/desktop/ms724397(v=vs.85).aspx
;; void WINAPI GetSystemTimeAsFileTime(
;;   _Out_ LPFILETIME lpSystemTimeAsFileTime
;; );
(cffi:defcfun ("GetSystemTimeAsFileTime" %GetSystemTimeAsFileTime) :void
  (lpSystemTimeAsFileTime (:pointer (:struct FILETIME))))

(defun GetSystemTimeAsFileTime ()
  "GetSystemTimeAsFileTime can only accurate to 100-nanosecond."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cffi:with-foreign-object (FILETIME '(:struct FILETIME))
    (%GetSystemTimeAsFileTime FILETIME)
    (cffi:with-foreign-slots ((dwLowDateTime dwHighDateTime)
                              FILETIME
                              (:struct FILETIME))
      (values dwHighDateTime
              dwLowDateTime))))

(defconstant +unix-epoch-filetime+ 116444736000000000)

(declaim (inline get-unix-time-us))
(defun get-unix-time-us ()
  "get unix-time in microseconds(us)."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (dwHighDateTime dwLowDateTime)
      (GetSystemTimeAsFileTime)
    (multiple-value-bind (us)
        (truncate (- (+ (ash dwHighDateTime 32)
                        dwLowDateTime)
                     +unix-epoch-filetime+)
                  10)
      us)))

(declaim (inline get-unix-time-ms))
(defun get-unix-time-ms ()
  "get unix-time in millisecond(ms)."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (ms)
      (truncate (get-unix-time-us) 1000)
    ms))
