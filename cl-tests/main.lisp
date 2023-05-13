#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)

(defpackage #:nrdl/tests
  (:use #:cl
        #:rove)
  (:import-from
    #:nrdl))
(in-package :nrdl/tests)

(deftest
  nested-to-alist
  (testing "empty"
           (ok (equal nil (nrdl:nested-to-alist nil)))
           (ok (equal "" (nrdl:nested-to-alist ""))))
  (testing "atomic values"
           (ok (equal "hi" (nrdl:nested-to-alist "hi")))
           (ok (equal 15 (nrdl:nested-to-alist 15)))
           (ok (equal t (nrdl:nested-to-alist t)))
           (ok (equal 'a (nrdl:nested-to-alist 'a)))
           (ok (equal :b (nrdl:nested-to-alist :b))))
  (testing "typical invocations"
           (ok
             (equal
               (let
                 ((a (make-hash-table)))
                 (setf (gethash 'a a) 1)
                 (setf (gethash 'b a) 2)
                 (setf (gethash 'c a) 3)
                 (nrdl:nested-to-alist
                   `(1 2 3 (4 5) 6 (7 (8 ,a)))))
               '(1 2 3 (4 5) 6 (7 (8 ((A . 1) (B . 2) (C . 3)))))))
           (ok (equal
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
                     (:ORIGIN . "thither")) (C 1 2 3 4 5))))))

(deftest
  parse-tests
  (testing "empty"
           (ok (signals (with-input-from-string (strm "")
                      (nrdl:parse-from strm)))))
  (testing "simple case"
           (ok (equal
                 '(:a)
                 (with-input-from-string (strm "[a]")
                   (nrdl:parse-from strm))
                 )))
  (testing "more general case"
           (ok (equal
                 (nrdl:nested-to-alist
                   (with-input-from-string
                     (strm
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

                       'force push'
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
                       }
                       ")
                     (nrdl:parse-from strm)))
'((:|FORCE PUSH| . "I sing because I'm happy")
 ("I am web mistress ming" . NRDL:FALSE)
 (:OTHER . "And I know
He's watching
Over me")
 (:POEM . "His eyee
is on
The sparrow")
 (:THE-SPARROWS . :HIS-EYE) (:THE-TREES . NRDL:FALSE) (:THE-WIND . "bullseye")
 (:THIS-SHOULD-STILL-WORK . 15.0)
 (:WENDOVER
  ((:ALSO . -1000) (:AND . 1.01) (:APPARENTLY . 10000) (:BUT . 1000)
   (:GAMBLING . 100) (:MUCH . -10) (:PARAMEDICS . -10000) (:SO . 1))
  ((:A . :FIRE) (:DIE . :IN)) 15 "this
that"
"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
  )
 ("i am mordac" . T) ("you are so wrong"))
)
 )))

(deftest
  json-generate-test
  (testing "simple example of json"
    (ok (equal
    (with-output-to-string (strm)
      (nrdl:generate-to strm
                        (alexandria:alist-hash-table
                          `((:a . 1)
                            (:b . (:x :y :z))
                            (:c . ("food" "for" "thought"))
                            (:d . ,nil)
                            (:e . nrdl:false)
                            (:f . t)
                            (:g . 0.87)))
                        :pretty-indent 4
                        :json-mode t))
"{
    \"a\": 1,
    \"b\": 2,
    \"c\": [
        \"food\",
        \"for\",
        \"thought\"
    ],
    \"d\": null,
    \"e\": false,
    \"f\": true,
    \"g\": 0.87
}"))))


        sparrow 4))


(deftest
  generate-test
  (testing "thorough example"
  (let ((sparrow (with-input-from-string
                     (strm
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

                       'force push'
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
                     (nrdl:parse-from strm))))
    (ok (equal
    (with-output-to-string (strm)
      (nrdl:generate-to strm sparrow :pretty-indent 4))
"{
    'force push' \"I sing because I'm happy\"
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
}"
)))))
