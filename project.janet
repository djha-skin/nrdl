# project.janet
#
# NRDL (Nestable Readable Document Language) Janet package.

(declare-project
  :name "nrdl"
  :description "Nestable Readable Document Language"
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :dependencies [])

(declare-source
  :prefix "nrdl"
  :source ["janet/src/main.janet"])

# jpm's declare-project registers a default `test` task that runs
# `run-tests` on the root `test/` directory. Our tests live in
# `janet/tests`, so point the existing rule's recipe there instead
# (the `task` macro would only append another thunk to it).
(def test-rule (get (dyn :rules) "test"))
(array/clear (get test-rule :recipe))
(array/push (get test-rule :recipe) (fn [] (run-tests "janet/tests")))
