(defpackage #:cl-datetime
  (:use #:cl)
  (:nicknames #:datetime #:dt)
  (:shadow #:defconstant)
  (:export #:+unix->universal-time+
           #:+unix->universal-time-ms+
           #:+unix->universal-time-us+
           #:+one-day-second-count+
           #:+one-day-millisecond-count+

           ;; universal-time in ms and us utils
           #:get-universal-time-ms
           #:get-universal-time-us

           ;; unix-time in second, ms and us utils
           #:get-unix-time
           #:get-unix-time-ms
           #:get-unix-time-us

           ;; universal-time to unix-time utils
           #:universal-time->unix-time
           #:universal-time-ms->unix-time-ms
           #:universal-time-us->unix-time-us

           ;; unix-time to universal-time utils
           #:unix-time->universal-time
           #:unix-time-ms->universal-time-ms
           #:unix-time-us->universal-time-us

           ;; decode utils
           #:decode-universal-time-ms
           #:decode-universal-time-us
           #:get-decoded-time-ms
           #:get-decoded-time-us

           ;; encode utils
           #:encode-universal-time-ms
           #:encode-universal-time-us

           ;; format utils
           #:format-integer
           #:datetime-formatter
           #:universal-time->yyyyMMddHHmmss
           #:universal-time->yyyyMMdd
           #:universal-time->yyMMdd
           #:universal-time->iso-time
           #:universal-time->yyyy-MM-dd
           #:universal-time->yy-MM-dd
           #:universal-time->yyyy/MM/dd
           #:universal-time->yy/MM/dd
           #:time-in-human-readable

           ;; parse utils
           #:iso-time->universal-time
           #:yyyyMMdd->universal-time
           #:yyyy-MM-dd->universal-time
           #:yyyy/MM/dd->universal-time

           ;; period utils
           #:today-begin-universal-time
           #:today-end-universal-time
           #:day-range-universal-time
           #:today-range-universal-time
           #:yestoday-range-universal-time
           #:tomorrow-range-universal-time

           ;; readtable utils
           #:datetime-readtable))
