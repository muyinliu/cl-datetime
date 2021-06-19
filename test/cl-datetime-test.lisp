(defpackage #:cl-datetime-test
  (:use :cl :prove))

(in-package :cl-datetime-test)

(plan nil)

(subtest "Testing get-universal-time-ms"
  (let ((universal-time (get-universal-time))
        (universal-time-ms (dt:get-universal-time-ms)))
    (is universal-time
        (truncate universal-time-ms 1000))))

(subtest "Testing get-universal-time-us"
  (let ((universal-time (get-universal-time))
        (universal-time-us (dt:get-universal-time-us)))
    (is universal-time
        (truncate universal-time-us 1000000))))

(subtest "Testing get-unix-time-ms"
  (let ((unix-time (dt:get-unix-time))
        (unix-time-ms (dt:get-unix-time-ms)))
    (is unix-time
        (truncate unix-time-ms 1000))))

(subtest "Testing get-unix-time-us"
  (let ((unix-time (dt:get-unix-time))
        (unix-time-us (dt:get-unix-time-us)))
    (is unix-time
        (truncate unix-time-us 1000000))))

(subtest "Testing decode-universal-time-ms"
  (is-values (dt:decode-universal-time-ms 3708914337135 -8)
             '(135 57 58 13 13 7 2017 3 NIL -8)))

(subtest "Testing encode-universal-time-ms"
  (is (dt:encode-universal-time-ms 135 57 58 13 13 7 2017 -8)
      3708914337135))

(subtest "Testing decode-universal-time-us"
  (is-values (dt:decode-universal-time-us 3708914337135397 -8)
             '(397 135 57 58 13 13 7 2017 3 NIL -8)))

(subtest "Testing encode-universal-time-us"
  (is (dt:encode-universal-time-us 397 135 57 58 13 13 7 2017 -8)
      3708914337135397))

(subtest "Testing format-integer"
  (is (with-output-to-string (stream)
        (dt:format-integer stream 2017 4 4))
      "2017")
  (is (with-output-to-string (stream)
        (dt:format-integer stream 2017 3 4))
      "017")
  (is (with-output-to-string (stream)
        (dt:format-integer stream 2017 2 4))
      "17")
  (is (with-output-to-string (stream)
        (dt:format-integer stream 2017 1 4))
      "7")
  (is (with-output-to-string (stream)
        (dt:format-integer stream 357 1 3 :from-end nil))
      "3")
  (is (with-output-to-string (stream)
        (dt:format-integer stream 357 2 3 :from-end nil))
      "35")
  (is-error (with-output-to-string (stream)
              (dt:format-integer stream 2017 0 4))
            'error)
  (is-error (with-output-to-string (stream)
              (dt:format-integer stream 2017 5 4))
            'error)
  (is (with-output-to-string (stream)
        (dt:format-integer stream 7 2 2))
      "07")
  (is (with-output-to-string (stream)
        (dt:format-integer stream 7 2 2 :from-end nil))
      "70"))

(subtest "Testing datetime-formatter"
  (let ((universal-time-us 3708914337135397))
    (subtest "Testing empty formatter"
      (is (format nil (dt:datetime-formatter ""))
          ""))
    (subtest "Testing escape formatter"
      (is (format nil (dt:datetime-formatter "'"))
          "")
      (is (format nil (dt:datetime-formatter "'''"))
          "'")
      (is (format nil (dt:datetime-formatter "''''"))
          "''")
      (is (format nil (dt:datetime-formatter "''_"))
          "'_")
      (is (format nil (dt:datetime-formatter "'y'"))
          "y")
      (is (format nil (dt:datetime-formatter "'_''_'"))
          "__"))
    (subtest "Testing year formatter"
      (is (format nil (dt:datetime-formatter "yyyy")
                  :time-us universal-time-us
                  :zone -8)
          "2017")
      (is (format nil (dt:datetime-formatter "yyy")
                  :time-us universal-time-us
                  :zone -8)
          "017")
      (is (format nil (dt:datetime-formatter "yy")
                  :time-us universal-time-us
                  :zone -8)
          "17")
      (is (format nil (dt:datetime-formatter "y")
                  :time-us universal-time-us
                  :zone -8)
          "7"))
    (subtest "Testing month formatter"
      (is (format nil (dt:datetime-formatter "MM")
                  :time-us universal-time-us
                  :zone -8)
          "07")
      (is (format nil (dt:datetime-formatter "M")
                  :time-us universal-time-us
                  :zone -8)
          "7"))
    (subtest "Testing date formatter"
      (is (format nil (dt:datetime-formatter "dd")
                  :time-us universal-time-us
                  :zone -8)
          "13")
      (is (format nil (dt:datetime-formatter "d")
                  :time-us universal-time-us
                  :zone -8)
          "3"))
    (subtest "Testing hour formatter"
      (is (format nil (dt:datetime-formatter "HH")
                  :time-us universal-time-us
                  :zone -8)
          "13")
      (is (format nil (dt:datetime-formatter "H")
                  :time-us universal-time-us
                  :zone -8)
          "3"))
    (subtest "Testing hour(12-hour) formatter"
      (is (format nil (dt:datetime-formatter "hh")
                  :time-us universal-time-us
                  :zone -8)
          "01")
      (is (format nil (dt:datetime-formatter "h")
                  :time-us universal-time-us
                  :zone -8)
          "1"))
    (subtest "Testing minute formatter"
      (is (format nil (dt:datetime-formatter "mm")
                  :time-us universal-time-us
                  :zone -8)
          "58")
      (is (format nil (dt:datetime-formatter "m")
                  :time-us universal-time-us
                  :zone -8)
          "8"))
    (subtest "Testing second formatter"
      (is (format nil (dt:datetime-formatter "ss")
                  :time-us universal-time-us
                  :zone -8)
          "57")
      (is (format nil (dt:datetime-formatter "s")
                  :time-us universal-time-us
                  :zone -8)
          "7"))
    (subtest "Testing millisecond formatter"
      (is (format nil (dt:datetime-formatter "SSS")
                  :time-us universal-time-us
                  :zone -8)
          "135")
      (is (format nil (dt:datetime-formatter "SS")
                  :time-us universal-time-us
                  :zone -8)
          "13")
      (is (format nil (dt:datetime-formatter "S")
                  :time-us universal-time-us
                  :zone -8)
          "1"))
    (subtest "Testing AM/PM formatter"
      (is (format nil (dt:datetime-formatter "a")
                  :time-us universal-time-us
                  :zone -8)
          "PM")
      (is (format nil (dt:datetime-formatter "a")
                  :time-us (* (encode-universal-time 59 59 11 1 1 2017 -8)
                              1000000)
                  :zone -8)
          "AM")
      (is (format nil (dt:datetime-formatter "a")
                  :time-us (* (encode-universal-time 0 0 12 1 1 2017 -8)
                              1000000)
                  :zone -8)
          "PM")
      (is (format nil (dt:datetime-formatter "a")
                  :time-us (* (encode-universal-time 59 59 23 1 1 2017 -8)
                              1000000)
                  :zone -8)
          "PM")
      (is (format nil (dt:datetime-formatter "a")
                  :time-us (* (encode-universal-time 0 0 0 1 1 2017 -8)
                              1000000)
                  :zone -8)
          "AM"))
    (subtest "Testing week days formatter"
      (is (format nil (dt:datetime-formatter "E")
                  :time-us universal-time-us
                  :zone -8)
          "Thu")
      (is (format nil (dt:datetime-formatter "EE")
                  :time-us universal-time-us
                  :zone -8)
          "Thu")
      (is (format nil (dt:datetime-formatter "EEE")
                  :time-us universal-time-us
                  :zone -8)
          "Thu")
      (is (format nil (dt:datetime-formatter "EEEE")
                  :time-us universal-time-us
                  :zone -8)
          "Thursday")
      (is (format nil (dt:datetime-formatter "EEEEE")
                  :time-us universal-time-us
                  :zone -8)
          "T")
      (is-error (format nil (dt:datetime-formatter "EEEEEEE")
                        :time-us universal-time-us
                        :zone -8)
                'error))
    (subtest "Testing timezone formatter"
      (is (format nil (dt:datetime-formatter "Z")
                  :time-us universal-time-us
                  :zone 0)
          "+0000")
      (is (format nil (dt:datetime-formatter "Z")
                  :time-us universal-time-us
                  :zone -8)
          "+0800")
      (is (format nil (dt:datetime-formatter "Z")
                  :time-us universal-time-us
                  :zone +8)
          "-0800")
      (is (format nil (dt:datetime-formatter "Z")
                  :time-us universal-time-us
                  :zone -12)
          "+1200")

      (is (format nil (dt:datetime-formatter)
                  :time-us universal-time-us
                  :zone 0)
          "2017-07-13T05:58:57.135")
      (is (format nil (dt:datetime-formatter)
                  :time-us universal-time-us
                  :zone -8)
          "2017-07-13T13:58:57.135")
      (is (format nil (dt:datetime-formatter)
                  :time-us universal-time-us
                  :zone +8)
          "2017-07-12T21:58:57.135")
      (is (format nil (dt:datetime-formatter)
                  :time-us universal-time-us
                  :zone -12)
          "2017-07-13T17:58:57.135"))))

(finalize)
