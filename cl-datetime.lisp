(in-package :dt)

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; constant variables

(defconstant +unix->universal-time-us+ 2208988800000000)
(defconstant +unix->universal-time-ms+ 2208988800000)
(defconstant +unix->universal-time+    2208988800)


;;; universal-time to unix-time utils

(defun universal-time->unix-time (universal-time)
  (- universal-time
     +unix->universal-time+))

(defun universal-time-ms->unix-time-ms (universal-time-ms)
  (- universal-time-ms
     +unix->universal-time-ms+))

(defun universal-time-us->unix-time-us (universal-time-us)
  (- universal-time-us
     +unix->universal-time-us+))


;;; unix-time to universal-time utils

(defun unix-time->universal-time (unix-time)
  (+ unix-time
     +unix->universal-time+))

(defun unix-time-ms->universal-time-ms (unix-time-ms)
  (+ unix-time-ms
     +unix->universal-time-ms+))

(defun unix-time-us->universal-time-us (unix-time-us)
  (+ unix-time-us
     +unix->universal-time-us+))


;;; universal-time utils

(defun get-universal-time-ms ()
  (unix-time-ms->universal-time-ms
   (get-unix-time-ms)))

(defun get-universal-time-us ()
  (unix-time-us->universal-time-us
   (get-unix-time-us)))


;;; unix-time utils

(defun get-unix-time ()
  (universal-time->unix-time
   (get-universal-time)))


;;; decode utils

(defun decode-universal-time-ms (universal-time-ms &optional time-zone)
  (multiple-value-bind (universal-time millisecond)
      (truncate universal-time-ms 1000)
    (multiple-value-bind (second minute hour date month year day daylight-p zone)
        (decode-universal-time universal-time time-zone)
      (values millisecond second minute hour date month year day daylight-p zone))))

(defun get-decoded-time-ms ()
  (decode-universal-time-ms (get-universal-time-ms)))


(defun decode-universal-time-us (universal-time-us &optional time-zone)
  (multiple-value-bind (universal-time remainder)
      (truncate universal-time-us 1000000)
    (multiple-value-bind (millisecond microsecond)
        (truncate remainder 1000)
      (multiple-value-bind (second minute hour date month year day daylight-p zone)
          (decode-universal-time universal-time time-zone)
        (values microsecond millisecond
                second minute hour date month year day daylight-p zone)))))

(defun get-decoded-time-us ()
  (decode-universal-time-us (get-universal-time-us)))


;;; encode utils

(defun encode-universal-time-ms (millisecond second minute hour date month year zone)
  (+ (* 1000 (encode-universal-time second minute hour date month year zone))
     millisecond))

(defun encode-universal-time-us (microsecond millisecond
                                 second minute hour date month year zone)
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
  (unless format-string
    (setf format-string "yyyy-MM-dd'T'HH:mm:ss.SSS"))
  (lambda (stream &key time time-ms time-us zone)
    (or (and time
             (setf time-us (* 1000000 time)))
        (and time-ms
             (setf time-us (* 1000 time-ms))))
    (unless time-us
      (setf time-us (get-universal-time-us)))
    (when zone
      (assert (<= -12 zone 11)))
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
                 (go peek-char))))))))

(defun datetime-formatter-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((format-string (read stream nil (values) t)))
    (datetime-formatter format-string)))

(named-readtables:defreadtable datetime-readtable
    (:merge :standard)
  (:dispatch-macro-char #\# #\_ #'datetime-formatter-reader))

)
