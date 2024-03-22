;;;; main.lisp -- Reference Implementation Parser Tests for NRDL in Common Lisp
;;;;
;;;; SPDX-FileCopyrightText: 2024 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT

#+(or)
(progn
  (declaim (optimize (speed 0) (space 0) (debug 3)))
  (progn
    (asdf:load-system "parachute")
    (asdf:load-system "com.djhaskin.nrdl")
    (asdf:test-system "com.djhaskin.nrdl")))

(in-package #:cl-user)

(defpackage #:com.djhaskin.nrdl/tests
  (:use #:cl)
  (:import-from
    #:org.shirakumo.parachute
    #:define-test
    #:true
    #:false
    #:fail
    #:is
    #:isnt
    #:is-values
    #:isnt-values
    #:of-type
    #:finish
    #:test)
  (:import-from
    #:com.djhaskin.nrdl)
  (:local-nicknames
    (#:parachute #:org.shirakumo.parachute)
    (#:nrdl #:com.djhaskin.nrdl)))

(in-package #:com.djhaskin.nrdl/tests)

(define-test nested-to-alist)

(define-test "nested-to-alist: empty cases"
  :parent nested-to-alist
  (is eq nil (nrdl:nested-to-alist nil))
  (is equal "" (nrdl:nested-to-alist "")))

(define-test "nested-to-alist: atomic values"
  :parent nested-to-alist
  (is equal "hi" (nrdl:nested-to-alist "hi"))
  (is equal 15 (nrdl:nested-to-alist 15))
  (is equal t (nrdl:nested-to-alist t))
  (is equal 'a (nrdl:nested-to-alist 'a))
  (is equal :b (nrdl:nested-to-alist :b)))

(define-test "nested-to-alist: typical invocations"
  :parent nested-to-alist
  (is
    equal
    (let
        ((a (make-hash-table)))
      (setf (gethash 'a a) 1)
      (setf (gethash 'b a) 2)
      (setf (gethash 'c a) 3)
      (nrdl:nested-to-alist
        `(1 2 3 (4 5) 6 (7 (8 ,a)))))
    '(1 2 3 (4 5) 6 (7 (8 ((A . 1) (B . 2) (C . 3))))))
  (is equal
      (let ((a (make-hash-table))
            (b (make-hash-table)))
        (setf (gethash :origin b) "thither")
        (setf (gethash :destination b) "yon")
        (setf (gethash 'a a) nil)
        (setf (gethash 'b a) b)
        (setf (gethash 'c a) '(1 2 3 4 5))
        (nrdl:nested-to-alist a))
      '((A)
        (B
          (:DESTINATION . "yon")
          (:ORIGIN . "thither")) (C 1 2 3 4 5))))

(define-test parse-tests)

(define-test "parse: empty"
  :parent parse-tests
  (handler-case
      (progn
        (with-input-from-string (strm "")
          (nrdl:parse-from strm))
        (fail "Should have thrown an error"))
    (t (sig) (true sig))))

(define-test "parse: simple"
  :parent parse-tests
  (is equal
      '(:a)
      (with-input-from-string (strm "[a]")
        (nrdl:parse-from strm))))

(defparameter *sparrow*
  "
  # What now brown cow
  {
  the-wind \"bullseye\"
  the-trees false
  the-sparrows his-eye
  poem
  # I don't know if you can hear me
  |His eyee
  # or if
  # you're even there
  |is on
  # I don't know if you can listen
  |The sparrow
  ^

  # to a gypsy's prayer

  this-should-still-work 15.0
  other
  |And I know
  |He's watching
  |Over me
  ^

  `force push`
  >I sing
  >because
  >I'm happy
  ^

  \"i am mordac\" true
  \"I am web mistress ming\" false
  \"you are so wrong\" null
  wendover [
  {
  so 1
  much -10
  gambling 100
  but 1000
  also -1000
  apparently 10000
  paramedics -10000
  and 1.01
  }
  {
  die in
  a fire
  }
  15
  |this
  |that
  ^
  \"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\"
  ]
  }")

(defparameter *sparrow-alist*
  `((:|FORCE PUSH|
                                                   . "I sing because I'm happy")
    ("I am web mistress ming")
    (:OTHER . ,(format nil "~@{~A~^~%~}"
                                                            "And I know"
                                                            "He's watching"
                                                            "Over me"))
    (:POEM . ,(format nil "~@{~A~^~%~}"
                                                           "His eyee"
                                                           "is on"
                                                           "The sparrow"))
    (:THE-SPARROWS
      . :HIS-EYE)
    (:THE-TREES)
    (:THE-WIND . "bullseye")
    (:THIS-SHOULD-STILL-WORK . 15.0)
    (:WENDOVER
      ((:ALSO . -1000)
       (:AND . 1.01)
       (:APPARENTLY . 10000)
       (:BUT . 1000)
       (:GAMBLING . 100)
       (:MUCH . -10)
       (:PARAMEDICS . -10000)
       (:SO . 1))
      ((:A
         . :FIRE)
       (:DIE
         . :IN))
      15 ,(format nil "~@{~A~^~%~}" "this" "that")
      ,(format
         nil "~@{~A~^ ~}"
         "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do"
         "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
         "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut"
         "aliquip ex ea commodo consequat. Duis aute irure dolor in"
         "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla"
         "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in"
         "culpa qui officia deserunt mollit anim id est laborum."))
    ("i am mordac" . T) ("you are so wrong" . CL:NULL)))

(define-test "parse: more general case"
  :parent parse-tests
           (is equal
                 (nrdl:nested-to-alist
                   (with-input-from-string
                     (strm *sparrow*)
                     (nrdl:parse-from strm)))
    *sparrow-alist*))

(define-test json-generate-test)

(define-test "json: simple example of json"
  :parent json-generate-test
    (is equal
    (with-output-to-string (strm)
      (nrdl:generate-to strm
                        (alexandria:alist-hash-table
                          `((:a . 1)
                            (:b . (:x :y :z))
                            (:c . ("food" "for" "thought"))
                            (:d . cl:null)
                            (:e . ,nil)
                            (:f . t)
                            (:g . 0.87)))
                        :pretty-indent 4
                        :json-mode t))
"{
    \"a\": 1,
    \"b\": [
        \"x\",
        \"y\",
        \"z\"
    ],
    \"c\": [
        \"food\",
        \"for\",
        \"thought\"
    ],
    \"d\": null,
    \"e\": false,
    \"f\": true,
    \"g\": 0.87
}"))

(defparameter *sparrow-object* (with-input-from-string
                                  (strm *sparrow*)
                                  (nrdl:parse-from strm)))

(defparameter *pretty-sparrow*
"{
    `force push` \"I sing because I'm happy\"
    \"I am web mistress ming\" false
    other
        |And I know
        |He's watching
        |Over me
        ^
    poem
        |His eyee
        |is on
        |The sparrow
        ^
    the-sparrows his-eye
    the-trees false
    the-wind \"bullseye\"
    this-should-still-work 15.0
    wendover [
        {
            also -1000
            and 1.01
            apparently 10000
            but 1000
            gambling 100
            much -10
            paramedics -10000
            so 1
        }
        {
            a fire
            die in
        }
        15
        |this
        |that
        ^
        >Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
        >eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad
        >minim veniam, quis nostrud exercitation ullamco laboris nisi ut
        >aliquip ex ea commodo consequat. Duis aute irure dolor in
        >reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
        >pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
        >culpa qui officia deserunt mollit anim id est
        >laborum.
        ^
    ]
    \"i am mordac\" true
    \"you are so wrong\" null
}")

(define-test generate-output)

(define-test "generate: thorough example"
  :parent generate-output
    (is equal (with-output-to-string (strm)
      (nrdl:generate-to strm *sparrow-object* :pretty-indent 4))
        *pretty-sparrow*))

(defparameter *test-intern-package*
  (defpackage #:com.djhaskin.nrdl/test-intern-package))

(defparameter *sparrow-alist-different-package*
  `((COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::|FORCE PUSH|
                                                   . "I sing because I'm happy")
    ("I am web mistress ming")
    (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::OTHER . ,(format nil "~@{~A~^~%~}"
                                                            "And I know"
                                                            "He's watching"
                                                            "Over me"))
    (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::POEM . ,(format nil "~@{~A~^~%~}"
                                                           "His eyee"
                                                           "is on"
                                                           "The sparrow"))
    (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::THE-SPARROWS
      . COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::HIS-EYE)
    (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::THE-TREES)
    (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::THE-WIND . "bullseye")
    (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::THIS-SHOULD-STILL-WORK . 15.0)
    (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::WENDOVER
      ((COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::ALSO . -1000)
       (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::AND . 1.01)
       (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::APPARENTLY . 10000)
       (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::BUT . 1000)
       (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::GAMBLING . 100)
       (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::MUCH . -10)
       (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::PARAMEDICS . -10000)
       (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::SO . 1))
      ((COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::A
         . COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::FIRE)
       (COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::DIE
         . COM.DJHASKIN.NRDL/TEST-INTERN-PACKAGE::IN))
      15 ,(format nil "~@{~A~^~%~}" "this" "that")
      ,(format
         nil "~@{~A~^ ~}"
         "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do"
         "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
         "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut"
         "aliquip ex ea commodo consequat. Duis aute irure dolor in"
         "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla"
         "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in"
         "culpa qui officia deserunt mollit anim id est laborum."))
    ("i am mordac" . T) ("you are so wrong" . CL:NULL)))

(define-test alternate-package)

(define-test "Alternate package: parse"
  :parent alternate-package
  (is equal
      (nrdl:nested-to-alist
        (with-input-from-string
            (strm *sparrow*)
          (let ((nrdl:*symbol-package* *test-intern-package*))
            (nrdl:parse-from strm))))
      *sparrow-alist-different-package*))
