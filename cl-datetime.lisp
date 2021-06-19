(in-package :dt)

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; constant variables

(defconstant +unix->universal-time-us+ 2208988800000000)
(defconstant +unix->universal-time-ms+ 2208988800000)
(defconstant +unix->universal-time+    2208988800)


;;; universal-time to unix-time utils

(declaim (inline universal-time->unix-time))
(defun universal-time->unix-time (universal-time)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (- universal-time
     +unix->universal-time+))

(declaim (inline universal-time-ms->unix-time-ms))
(defun universal-time-ms->unix-time-ms (universal-time-ms)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (- universal-time-ms
     +unix->universal-time-ms+))

(declaim (inline universal-time-us->unix-time-us))
(defun universal-time-us->unix-time-us (universal-time-us)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (- universal-time-us
     +unix->universal-time-us+))


;;; unix-time to universal-time utils

(declaim (inline unix-time->universal-time))
(defun unix-time->universal-time (unix-time)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (+ unix-time
     +unix->universal-time+))

(declaim (inline unix-time-ms->universal-time-ms))
(defun unix-time-ms->universal-time-ms (unix-time-ms)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (+ unix-time-ms
     +unix->universal-time-ms+))

(declaim (inline unix-time-us->universal-time-us))
(defun unix-time-us->universal-time-us (unix-time-us)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (+ unix-time-us
     +unix->universal-time-us+))


;;; universal-time utils

(declaim (inline get-universal-time-ms))
(defun get-universal-time-ms ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unix-time-ms->universal-time-ms
   (get-unix-time-ms)))

(declaim (inline get-universal-time-us))
(defun get-universal-time-us ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unix-time-us->universal-time-us
   (get-unix-time-us)))


;;; unix-time utils

(declaim (inline get-unix-time))
(defun get-unix-time ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (universal-time->unix-time
   (get-universal-time)))


;;; decode utils

(defun decode-universal-time-ms (universal-time-ms &optional time-zone)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (universal-time millisecond)
      (truncate universal-time-ms 1000)
    (multiple-value-bind (second minute hour date month year day daylight-p zone)
        (decode-universal-time universal-time time-zone)
      (values millisecond second minute hour date month year day daylight-p zone))))

(defun get-decoded-time-ms ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (decode-universal-time-ms (get-universal-time-ms)))


(defun decode-universal-time-us (universal-time-us &optional time-zone)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (universal-time remainder)
      (truncate universal-time-us 1000000)
    (multiple-value-bind (millisecond microsecond)
        (truncate remainder 1000)
      (multiple-value-bind (second minute hour date month year day daylight-p zone)
          (decode-universal-time universal-time time-zone)
        (values microsecond millisecond
                second minute hour date month year day daylight-p zone)))))

(defun get-decoded-time-us ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (decode-universal-time-us (get-universal-time-us)))


;;; encode utils

(defun encode-universal-time-ms (millisecond second minute hour date month year zone)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (+ (* 1000 (encode-universal-time second minute hour date month year zone))
     millisecond))

(defun encode-universal-time-us (microsecond millisecond
                                 second minute hour date month year zone)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (+ (* 1000000 (encode-universal-time second minute hour date month year zone))
     (* 1000 millisecond)
     microsecond))

;;; format utils

(defun format-integer (stream number length max-length
                       &key (from-end t))
  (declare (optimize (speed 3) (safety 0))
           (integer number length max-length))
  (assert (<= 1 length max-length))
  (let* ((number-string (write-to-string number))
         (number-length (length number-string)))
    (declare (optimize (speed 3) (safety 0))
             (dynamic-extent number-string number-length)
             (string number-string)
             (integer number-length))
    (if (= length number-length)
        (write-string number-string stream)
        (if from-end
            (if (< number-length length)
                (progn
                  (dotimes (var (- length number-length))
                    (declare (optimize (speed 3) (safety 0)))
                    (write-char #\0 stream))
                  (write-string number-string stream))
                (write-string (subseq number-string (- number-length length)) stream))
            (if (< number-length length)
                (progn
                  (write-string number-string stream)
                  (dotimes (var (- length number-length))
                    (write-char #\0 stream)))
                (write-string (subseq number-string 0 length) stream))))))

(defun format-week-day (stream day char-count)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (write-string (case char-count
                  ((1 2 3)
                   (ccase day
                     (0 "Mon")
                     (1 "Tue")
                     (2 "Wed")
                     (3 "Thu")
                     (4 "Fri")
                     (5 "Sat")
                     (6 "Sun")))
                  (4
                   (ccase day
                     (0 "Monday")
                     (1 "Tuesday")
                     (2 "Wednesday")
                     (3 "Thursday")
                     (4 "Friday")
                     (6 "Sunday")
                     (5 "Saturday")))
                  (5
                   (ccase day
                     (0 "M")
                     (1 "T")
                     (2 "W")
                     (3 "T")
                     (4 "F")
                     (5 "S")
                     (6 "S")))
                  (t
                   (error "too many pattern letters: E")))
                stream))

(defun format-datetime-char (stream escape-p prev-char char-count
                             microsecond
                             millisecond
                             second minute hour date month year day
                             daylight-p zone)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignorable microsecond daylight-p))
  (case prev-char
    (#\y (format-integer stream year char-count 4))
    (#\M (format-integer stream month char-count 2))
    (#\d (format-integer stream date char-count 2))
    (#\H (format-integer stream hour char-count 2))
    (#\h (format-integer stream (let ((remainder (mod hour 12)))
                                  (if (zerop remainder)
                                      12
                                      remainder))
                         char-count 2))
    (#\m (format-integer stream minute char-count 2))
    (#\s (format-integer stream second char-count 2))
    (#\S (format-integer stream millisecond char-count 3
                         :from-end nil))
    (#\a (if (>= hour 12)
             (write-string "PM" stream)
             (write-string "AM" stream)))
    (#\E (format-week-day stream day char-count))
    (#\Z (if (plusp zone)
             (write-char #\- stream)
             (write-char #\+ stream))
         (format stream "~2,'0d00" (abs zone)))
    (t   (when prev-char
           (when (not (and (eq #\' prev-char)
                           (not escape-p)))
             (dotimes (var char-count)
               (write-char prev-char stream)))))))

(defun datetime-formatter (&optional format-string)
  "Date and time formatter. return a function:
  (lambda (stream &key time time-ms time-us zone))"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless format-string
    (setf format-string "yyyy-MM-dd'T'HH:mm:ss.SSS"))
  (lambda (stream &key time time-ms time-us zone)
    (when zone
      (assert (<= -12 zone 11)))
    (let ((time-us (cond (time
                          (* 1000000 time))
                         (time-ms
                          (* 1000 time-ms))
                         (time-us
                          time-us)
                         (t
                          (get-universal-time-us)))))
      (multiple-value-bind (microsecond
                            millisecond
                            second minute hour date month year day
                            daylight-p zone)
          (decode-universal-time-us time-us zone)
        (let ((char-list (concatenate 'list format-string))
              (char nil)
              (char-count 0)
              (prev-char nil)
              (escape-p nil)
              (end-p nil))
          (tagbody
           peek-char
             (setf prev-char char)
             (setf char (car char-list))
             (setf char-list (rest char-list))
             (go deal)
           deal
             (when (eq #\' prev-char)
               (setf escape-p (not escape-p)))
             (if escape-p
                 (unless (and (eq #\' char)
                              (not (eq #\' prev-char)))
                   (when char
                     (princ char stream)))
                 (if (eq char prev-char)
                     (incf char-count)
                     (progn
                       (format-datetime-char stream escape-p prev-char char-count
                                             microsecond
                                             millisecond
                                             second minute hour date month year day
                                             daylight-p zone)
                       (setf char-count 1))))
             (if (car char-list)
                 (go peek-char)
                 (unless end-p
                   (setf end-p t)
                   (go peek-char)))))))))

(defun datetime-formatter-reader (stream subchar arg)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (ignore subchar arg))
  (let ((format-string (read stream nil (values) t)))
    (datetime-formatter format-string)))

(named-readtables:defreadtable datetime-readtable
    (:merge :standard)
  (:dispatch-macro-char #\# #\_ #'datetime-formatter-reader))

)

;;; datetime format utils

(defun universal-time->yyyyMMddHHmmss (universal-time)
  "Format universal-time in format yyyyMMddHHmmss, like 20140330210342"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (s m h d mm y)
      (decode-universal-time universal-time)
    (format nil "~A~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d" y mm d h m s)))

(defun universal-time->yyyyMMdd (universal-time)
  "Format universal-time in format yyyyMMdd, like 20140330"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (s m h d mm y)
      (decode-universal-time universal-time)
    (declare (ignore s m h))
    (format nil "~A~2,'0d~2,'0d" y mm d)))

(defun universal-time->yyMMdd (universal-time)
  "Format universal-time in format yyMMdd, like 140330"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (s m h d mm y)
      (decode-universal-time universal-time)
    (declare (ignore s m h))
    (format nil "~2,'0d~2,'0d~2,'0d" (mod y 100) mm d)))

(defun universal-time->iso-time (universal-time)
  "Format universal-time in format yyyy-MM-dd HH:mm:ss, like 2014-03-30 21:03:42)"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (s m h d mm y)
      (decode-universal-time universal-time)
    (format nil
            "~A-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            y mm d h m s)))

(defun universal-time->yyyy-MM-dd (universal-time)
  "Format universal-time in format yyyy-MM-dd, like 2014-03-30"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (s m h d mm y)
      (decode-universal-time universal-time)
    (declare (ignore s m h))
    (format nil "~A-~2,'0d-~2,'0d" y mm d)))

(defun universal-time->yyyy/MM/dd (universal-time)
  "Format universal-time in format yyyy/MM/dd, like 2014/03/30"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (s m h d mm y)
      (decode-universal-time universal-time)
    (declare (ignore s m h))
    (format nil "~A/~2,'0d/~2,'0d" y mm d)))

(defun universal-time->yy-MM-dd (universal-time)
  "Format universal-time in format yy-MM-dd, like 14-03-30"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (s m h d mm y)
      (decode-universal-time universal-time)
    (declare (ignore s m h))
    (format nil "~2,'0A-~2,'0d-~2,'0d" (mod y 100) mm d)))

(defun universal-time->yy/MM/dd (universal-time)
  "Format universal-time in format yyMM/dd, like 14/03/30"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (s m h d mm y)
      (decode-universal-time universal-time)
    (declare (ignore s m h))
    (format nil "~2,'0A/~2,'0d/~2,'0d" (mod y 100) mm d)))

(defun time-in-human-readable (second
                               &key
                                 (unit-list '("秒" "分钟" "小时" "天" "年"))
                                 (chinese-p t))
  "Time(seconds count) in human-readable."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((scale-list (list 60 60 24 365))
        remainder-list)
    (with-output-to-string (stream)
      (loop
        with scale-list-length = (length scale-list)
        and scale-index = 0
        and pre-consult = second
        do (let ((scale (if (> scale-index (1- scale-list-length))
                            nil
                            (nth scale-index scale-list))))
             (if (and scale
                      (>= pre-consult scale))
                 (multiple-value-bind (consult remainder)
                     (floor pre-consult scale)
                   (push remainder remainder-list)
                   (incf scale-index)
                   (setf pre-consult consult))
                 (progn
                   (push pre-consult remainder-list)
                   (loop-finish)))))
      (format stream (format nil "~~{~~A~~A~@[~~^ ~]~~}" (not chinese-p))
              (loop
                for remainder in remainder-list
                and unit in (reverse (subseq unit-list 0 (length remainder-list)))
                when (> remainder 0)
                  collect remainder
                when (> remainder 0)
                  collect unit)))))

;;; datetime parse utils

(defun iso-time->universal-time (iso-time-string)
  "Convert iso-time-string\(in format yyyy-MM-dd HH:mm:ss, like 2016-01-17 08:46:47\) to Universal-Time."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (ppcre:register-groups-bind (year month day hour minute second)
      ("^(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2}):(\\d{2})$"
       iso-time-string)
    (check-type second string)
    (check-type minute string)
    (check-type hour   string)
    (check-type day    string)
    (check-type month  string)
    (check-type year   string)
    (encode-universal-time (parse-integer second)
                           (parse-integer minute)
                           (parse-integer hour)
                           (parse-integer day)
                           (parse-integer month)
                           (parse-integer year))))

(defun yyyyMMdd->universal-time (date-string)
  "Convert date-string\(in format yyyyMMdd, like 20160117\) to Universal-Time."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (ppcre:register-groups-bind (year month day)
      ("^(\\d{4})(\\d{2})(\\d{2})$"
       date-string)
    (check-type day   string)
    (check-type month string)
    (check-type year  string)
    (encode-universal-time 0
                           0
                           0
                           (parse-integer day)
                           (parse-integer month)
                           (parse-integer year))))

(defun yyyy-MM-dd->universal-time (date-string)
  "Convert date-string\(in format yyyy-MM-dd, like 2016-01-17\) to Universal-Time."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (ppcre:register-groups-bind (year month day)
      ("^(\\d{4})-(\\d{2})-(\\d{2})$"
       date-string)
    (check-type day   string)
    (check-type month string)
    (check-type year  string)
    (encode-universal-time 0
                           0
                           0
                           (parse-integer day)
                           (parse-integer month)
                           (parse-integer year))))

(defun yyyy/MM/dd->universal-time (date-string)
  "Convert date-string\(in format yyyy/MM/dd, like 2016/01/17\) to Universal-Time."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (ppcre:register-groups-bind (year month day)
      ("^(\\d{4})/(\\d{2})/(\\d{2})$"
       date-string)
    (check-type day   string)
    (check-type month string)
    (check-type year  string)
    (encode-universal-time 0
                           0
                           0
                           (parse-integer day)
                           (parse-integer month)
                           (parse-integer year))))

;;; datetime period utils

(defconstant +one-day-second-count+ 86400)
(defconstant +one-day-millisecond-count+ 86400000)

(defun today-begin-universal-time ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (second minute hour date month year)
      (get-decoded-time)
    (declare (ignore second minute hour))
    (encode-universal-time 0 0 0 date month year)))

(defun today-end-universal-time ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (+ (today-begin-universal-time) +one-day-second-count+))

(defun day-range-universal-time (offset)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((today-begin-universal-time (today-begin-universal-time)))
    (values (+ today-begin-universal-time (* offset +one-day-second-count+))
            (+ today-begin-universal-time (* (1+ offset) +one-day-second-count+)))))

(defun today-range-universal-time ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (day-range-universal-time 0))

(defun yestoday-range-universal-time ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (day-range-universal-time -1))

(defun tomorrow-range-universal-time ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (day-range-universal-time 1))
