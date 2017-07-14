(defsystem "cl-datetime-test"
  :name "cl-datetime-test"
  :description "test case for cl-datetime"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :depends-on ("cl-datetime" "prove")
  :defsystem-depends-on ("prove-asdf")
  :components ((:module "test"
                        :serial t
                        :components ((:file "cl-datetime-test"))))
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
