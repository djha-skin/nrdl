(defsystem "com.djhaskin.nrdl"
  :version "0.4.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "alexandria"
               "trivial-features"
               )
  :components ((:module "cl"
          :components
          ((:file "main"))))
  :description "Nestable Readable Document Language"
  :in-order-to (
                (test-op (test-op "com.djhaskin.nrdl/tests"))))

(defsystem "com.djhaskin.nrdl/tests"
  :version "0.4.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "com.djhaskin.nrdl"
               "parachute")
  :components ((:module "cl-tests"
                :components
                ((:file "main"))))
  :description "Test system for NRDL"
  :perform (asdf:test-op (op c)

                         (uiop:symbol-call
                           :parachute
                           :test :com.djhaskin.nrdl/tests)))
