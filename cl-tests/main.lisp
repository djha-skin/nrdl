#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)

(defpackage #:nrdl/tests
  (:use #:cl
        #:rove)
  (:import-from
    #:nrdl)
  (:import-from
    #:cl-ppcre))
(in-package :nrdl/tests)

(defun positive-dec (n) (declare (number n)) (if (> n 0) (- n 1) 0))

(defun broken-dec (n) (declare (number n)) (if (>= n 0) (- n 1) 0))

;(deftest
;  parse-test
;  (testing "repeatedly-eq"
;  (signals (nrdl:foo))
;  (ok (equal (nrdl:bar 3) 3))))

