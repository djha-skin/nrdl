(defsystem "nrdl"
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "alexandria"
               )
  :components ((:module "cl"
          :components
          ((:file "main"))))
  :description "Nestable Readable Document Language"
  :in-order-to ((test-op (test-op "nrdl/tests"))))

(defsystem "nrdl/tests"
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
      "alexandria"
      "rove")
  :components ((:module "cl-tests"
                :components
                ((:file "main"))))
  :description "Test system for nrdl"
  :perform (test-op (op c) (symbol-call :rove :run c)))
