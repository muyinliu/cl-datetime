(asdf:defsystem "cl-datetime"
  :name "datetime"
  :description "Date and time library for Common Lisp."
  :version "0.0.1"
  :author "Muyinliu Xing <muyinliu@gamil.com>"
  :license "ISC"
  :depends-on ("cffi"
               "cl-ppcre"
               "named-readtables")
  :in-order-to ((test-op (test-op "cl-datetime-test")))
  :serial t
  :components ((:file "packages")
               (:file "utils")
               #+(or bsd unix freebsd linux)
               (:module "unix"
                        :serial t
                        :components ((:file "cl-datetime-gettimeofday")))
               #+win32
               (:module "win32"
                        :serial t
                        :components ((:file "cl-datetime-get-system-time-as-file-time")))
               (:file "cl-datetime")))
