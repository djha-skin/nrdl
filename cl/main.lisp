;;;; main.lisp -- Reference Implementation Parser for NRDL in Common Lisp
;;;;
;;;; SPDX-FileCopyrightText: 2024 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT
;;;;

#+(or)

(progn
  (declaim (optimize (speed 0) (space 0) (debug 3)))
           (asdf:load-system "alexandria"))

(in-package #:cl-user)

(defpackage
  #:com.djhaskin.nrdl (:use #:cl)
  (:documentation
    "
    Nestable Readable Document Language
    ")
    (:import-from #:alexandria)
    (:export
      extract-error
      parse-from
      generate-to
      nested-to-alist
      string-symbol
      symbol-string
      *symbol-package*
      *symbol-deserialize-case*
      *symbol-serialize-case*))

(in-package #:com.djhaskin.nrdl)

(defconstant +eof+ :eof)

(deftype streamable ()
  '(or boolean stream))

(deftype streamed ()
  `(or character (member ,+eof+)))


(defun nameof (c)
  (cond c
        ((eq c +eof+)
         (format nil "EOF"))
        ((typep gotc 'character)
         (format nil "~:C" c))
        (t
         (format nil "~A" c))))


(define-condition extract-error (error)
  ((expected-chars :initarg :expected-chars :reader expected-chars)
   (got-char :initarg :got-char :reader got-char))
  (:report
   (lambda (c s)
     (let* ((gotc (got-char c))
            (got-message
              ))
     (let ((got-message
             (typecase (got-char c)
               (
     (if (zerop (length c))
         (format 

     (loop for c in (expected-chars c)
           do
           (typecase c
             (character 
     (format s
             "Expected ~v[nothing~;~:;one of ~]~{`~:C`~^~#[~; or ~:;, ~]~}; got `~:C`"
             (list-length (expected-chars c))
             (expected-chars c)
             (got-char c)))))

;; TODO write a function to construct different types of errors out of the above
;; condition


(defun expected-whitespace (chr)
  (declare (type streamed chr))
  (error 'extract-error
         :expected-chars '(#\Space #\Tab #\Newline #\Return #\Page)
         :got-char chr))

(defun peek-chr (strm)
  (declare (type streamable strm))
  (peek-char nil strm nil +eof+ nil))

(defun read-chr (strm)
  (declare (type streamable strm))
  (read-char strm nil +eof+ nil))

(defun must-read-chr (strm)
  (declare (type streamable strm))
  (read-char strm))

(defun number-start-p (chr)
  (declare (type character chr))
  (or
    (char= chr #\-)
    (char= chr #\.)
    (digit-char-p chr)))

(defun number-char-p (chr)
  (declare (type character chr))
  (or
    (number-start-p chr)
    (char= chr #\+)
    (char= chr #\E)
    (char= chr #\e)))

#+(or)
(equal
  (build-string '(#\c #\b #\a))
  "abc")
#+or
(equal (build-string nil) "")

(defun build-string (lst)
  (declare (type list lst))
  (let* ((size (length lst))
         (building (make-string size)))
    (loop for l in lst
          for j from (- size 1) downto 0
          do
          (setf (elt building j) l))
    building))

(defun extract-number (strm chr)
  (declare (type streamable strm)
           (type streamed chr))
  (let ((building nil)
        (last-read chr))
    (loop while (and
                  (not (eq last-read +eof+))
                  (number-char-p last-read))
          do
          (push last-read building)
          (read-chr strm)
          (setf last-read (peek-chr strm)))
    (read-from-string (build-string building) nil nil)))

(defun extract-quoted
  (strm chr quote-char)
  (declare (type streamable strm)
           (type streamed chr)
           (type character quote-char))
  (declare (ignore chr))
  (must-read-chr strm)
  (let ((last-read (must-read-chr strm))
        (building nil))
    (loop while (and (not (eq last-read +eof+))
                     (char/= last-read quote-char))
          do
          (if (char= last-read #\\)
            (progn
              (setf last-read (must-read-chr strm))
              (cond
                    ((eql last-read quote-char)
                     (push quote-char building))
                    ((char= last-read #\\)
                     (push last-read building))
                    ((char= last-read #\/)
                     (push last-read building))
                    ((char= last-read #\b)
                     (push #\Backspace building))
                     ((char= last-read #\f)
                      (push #\Page building))
                     ((char= last-read #\n)
                      (push #\Newline building))
                     ((char= last-read #\r)
                      (push #\Return building))
                     ((char= last-read #\t)
                      (push #\Tab building))
                     ((char= last-read #\u)
                      (push
                        (code-char
                          (let ((build-ordinal (make-string 6)))
                            (setf (elt build-ordinal 0) #\#)
                            (setf (elt build-ordinal 1) #\X)
                            (setf (elt build-ordinal 2) (must-read-chr strm))
                            (setf (elt build-ordinal 3) (must-read-chr strm))
                            (setf (elt build-ordinal 4) (must-read-chr strm))
                            (setf (elt build-ordinal 5) (must-read-chr strm))
                            (read-from-string build-ordinal nil nil)))
                        building))
                     (t (error 'nrdl-error "Unknown escape char: ~A"
                               last-read))))
            (push last-read building))
          (setf last-read (must-read-chr strm)))
    (build-string building)))

(defconstant +start-verbatim+ #\|)
(defconstant +start-prose+ #\>)
(defconstant +start-comment+ #\#)

(defun blankspace-p (chr)
  (declare (type character chr))
  (or
    (char= chr #\Tab)
    (char= chr #\Space)
    ))

(defun whitespace-p (chr)
  (declare (type character chr))
    (or
      (char= chr #\Newline)
      (char= chr #\Return)
      (char= chr #\Page)
      (blankspace-p chr)))

(defun sepchar-p (chr)
  (declare (type character chr))
  (or (whitespace-p chr)
      (char= chr #\,)
      (char= chr #\:)))

(defun guarded-sepchar-p (chr)
  (declare (type (or null character) chr))
    (unless (null chr)
      (sepchar-p chr)))

(defun guarded-blankspace-p (chr)
  (declare (type (or null character) chr))
    (unless (null chr)
      (blankspace-p chr)))

(defun extract-comment (strm)
  (declare (type streamable strm))
  (loop with last-read = (read-chr strm)
        while (and
                (not (eq last-read +eof+))
                (not (char= last-read #\Newline)))
        do
        (setf last-read (read-chr strm))
        finally
        (return last-read)))

(defun extract-sep (strm chr pred)
  (declare (type streamable strm)
           (type streamed chr)
           (type function pred))
  (loop with just-read = nil
        with next = chr
    do
    (cond
      ((eq next +eof+)
       (return just-read))
      ((eql next +start-comment+)
       (setf just-read (extract-comment strm)))
      ((funcall pred next)
       (setf just-read (must-read-chr strm)))
      (t
        (return just-read)))
    (setf next (peek-chr strm))))

(defun extract-list-sep (strm chr)
  (declare (type streamable strm)
           (type streamed chr))
  (extract-sep strm chr #'guarded-sepchar-p))

(defun extract-blob-sep (strm chr)
  (declare (type streamable strm)
           (type streamed chr))
  (extract-sep strm chr #'guarded-blankspace-p))
#|
(extract-multiline-blob t #\|)
|one
#and then
    # some of
        #
   | two
#this
|  three

(extract-multiline-blob t #\>)
>one
#and then
    # some of
        #
   > two
#this
>  three

|#

(defun extract-multiline-blob (strm chr)
  "
  Grabs the string after the multiline marker
  and grabs subsequent prefixed strings
  until a non-prefix character, EOF, or two new-lines in a row.
  "
  (declare (type streamable strm)
           (type streamed chr))
  (loop named toplevel
        with last-read = nil
        with next = chr
        with building = nil
        while (and
                (not (eq next +eof+))
                (eql next chr)
                (not (char= next #\^)))
        do
        (setf last-read (read-chr strm))
        (setf next (peek-chr strm))
        (loop named getline
              while (and (not (eq next +eof+))
                         (not (char= next #\Newline)))
              do
              (setf last-read (read-chr strm))
              (push last-read building)
              (setf next (peek-chr strm)))
        (when (eq next +eof+)
          (return-from toplevel
                       (build-string building)))
        (setf last-read (read-chr strm))
        (if (eql chr +start-verbatim+)
          (push last-read building)
          (push #\Space building))
        (let ((sep-result
                (extract-blob-sep strm (peek-chr strm))))
          (when sep-result
            (setf last-read sep-result)))
        (setf next (peek-chr strm))

        finally (progn
                  (unless (char= next #\^)
                    (error 'nrdl-error "Must end multiline blob with a caret"))
                  (read-chr strm)
                  (return-from toplevel (build-string (cdr building))))))

(defun extract-quoted-blob (strm chr)
  (declare (type streamable strm)
           (type streamed chr))
  (extract-quoted strm chr #\"))

#+(or)
(do
  (string-invertcase "")
  (string-invertcase "A")
  (string-invertcase "a")
  (string-invertcase " ")
  (string-invertcase "my heart's a stereo")
  (string-invertcase "My heart's a stereo"))

(defun string-invertcase
  (str)
  (declare (type string str))
  (let ((operable (remove-if-not #'both-case-p str)))
    (if (= (length operable) 0)
      str
      (let* ((first-upper-case (upper-case-p (elt operable 0)))
             (uneven-case
               (when (> (length operable) 1)
                      (reduce
                        (lambda (c v)
                          (or c v))
                        (map
                          'vector
                          (lambda (x)
                            (not (eql (upper-case-p x) first-upper-case)))
                          (subseq operable 1))))))
        (if uneven-case
          str
          (if first-upper-case
            (string-downcase str)
            (string-upcase str)))))))

;;; Variable used to determine the case of a string
;;; used for the name of a new symbol upon deserialization.
;;;
;;; Set to `(readtable-case *readtable*)` by default.
(defparameter *symbol-deserialize-case*
  (readtable-case *readtable*))

;;; Variable used to determine the case of a string
;;; generated from the name of a symbol upon serialization.
;;; Can have the same values as the output of `readtable-case`:
;;; `:upcase`, `:downcase`, `:preserve`, and `:invert`,
;;; with the same effects.
;;;
;;; Unlike its cousin, I can't just give this thing a
;;; default based on the readtable,
;;;
;;; and the printer has no notion of this. It just prints whatever case
;;; was given originally. In effect, the printer is set to `:preserve` by
;;; default.
;;;
;;; This is a default which is not useful in my case, I think.
;;;
;;; Therefore, by default, it will be set to `:downcase`, which addresses
;;; a much more common need.
(defparameter *symbol-serialize-case*
  :downcase)

(defun
  symbol-string-prep (str case-guide)
  "
  Attempts to prepare a string for interning according to the normal
  ANSI rules for internment of literal symbols.
  "
  (declare (type string str)
           (type keyword case-guide))
  (cond ((eql case-guide :upcase) (string-upcase str))
        ((eql case-guide :downcase) (string-downcase str))
        ((eql case-guide :preserve) str)
        ((eql case-guide :invert) (string-invertcase str))
        (t (string-upcase str))))

(defparameter *symbol-package* (find-package :KEYWORD))

(defun
  string-symbol (str &optional (case-guide *symbol-deserialize-case*))
  "
  Creates a keyword from a string.

  Creates a keyword following `(readtable-case *readtable*)` rules.

  If this package's var `*symbol-case*` is `:upcase`, the string is upcased.
  If it is `:downcase`, the string is downcased.
  If it is `:preserve`, the string's case is preserved.
  If it is `:invert`, the string's case is inverted as long as all caseable
  characters are already of uniform case.

  The variable `*symbol-case*` takes as its default the current value of
  `(readtable-case *readtable*)` at package load time.
  "
  (declare (type string str)
           (type keyword case-guide))
  (intern
    (symbol-string-prep str case-guide)
    *symbol-package*))

(defun symbol-string
  (sym &optional (case-guide *symbol-serialize-case*))
  (declare (type symbol sym)
           (type keyword case-guide))
  (cond ((eql sym 't) "true")
        ((eql sym 'nil) "false")
        ((eql sym 'cl:null) "null")
        (t (symbol-string-prep (symbol-name sym) case-guide))))

(defun bareword-start-p (chr)
  (declare (type streamed chr))
  (or
    (and (typep chr 'character)
         (alpha-char-p chr))
    (char= chr #\_)
    (char= chr #\=)
    (char= chr #\>)
    (char= chr #\@)
    (char= chr #\$)
    (char= chr #\%)
    (char= chr #\&)
    (char= chr #\*)
    (char= chr #\+)
    (char= chr #\/)
    (char= chr #\=)))

(defun bareword-middle-p (chr)
  (declare (type streamed chr))
  (or
    (bareword-start-p chr)
    (and (typep chr 'character)
         (digit-char-p chr))
    (char= chr #\<)
    (char= chr #\!)
    (char= chr #\?)
    (char= chr #\.)
    (char= chr #\-)))

(defun convert-to-symbol (final-string)
  (declare (type string final-string))
  (cond ((string= final-string "t")
                  t)
        ((string= final-string "nil")
         'cl:null)
        ((string= final-string "true")
         t)
        ((string= final-string "false")
         nil)
        ((string= final-string "null")
         'cl:null)
        (t (string-symbol final-string))))

(defun extract-bare-symbol (strm chr)
  (declare (type streamable strm)
           (type streamed chr))
  (loop with building = nil
        with last-read = nil
        with next = chr
        while (and
                (not (eq next +eof+))
                (bareword-middle-p next))
        do
        (setf last-read (read-chr strm))
        (push last-read building)
        (setf next (peek-chr strm))
        finally
        (return
          (convert-to-symbol (build-string building)))))

(defun extract-quoted-symbol (strm chr)
  (declare (type streamable strm)
           (type streamed chr))
  (convert-to-symbol
    (extract-quoted strm chr #\`)))

(defun extract-value (strm chr)
  (declare (type streamable strm)
           (type streamed chr))
  (cond
        ((eq chr +eof+) (error 'nrdl-error "No value"))
        ((char= chr #\{)
         (extract-hash strm chr))
        ((char= chr #\[)
         (extract-array strm chr))
        ((char= chr #\")
         (extract-quoted-blob strm chr))
        ((char= chr #\`)
         (extract-quoted-symbol strm chr))
        ((or
           (eql chr +start-verbatim+)
           (eql chr +start-prose+))
         (extract-multiline-blob strm chr))
        ((number-start-p chr)
         (extract-number strm chr))
        ((bareword-start-p chr)
         (extract-bare-symbol strm chr))
        (t (error 'nrdl-error "Invalid token starting with character `~A`"
                  chr))))

(defun extract-array (strm chr)
  (declare (type streamable strm)
           (type streamed chr)
           (ignore chr))
  (read-chr strm)
  (loop with building = nil
        with found-sep = t
        with last-read = (extract-list-sep strm (peek-chr strm))
        with next = (peek-chr strm)
        while (and
                (not (eq next +eof+))
                (not (char= next #\])))
        do
        (unless found-sep
          (error
            'nrdl-error
            "No separating whitespace found at character `~A`."
            next)
          )
        (push (extract-value strm next) building)
        (setf last-read (extract-list-sep strm (peek-chr strm)))
        (setf found-sep (guarded-sepchar-p last-read))
        (setf next (peek-chr strm))
        finally
        (progn (when (and
                       (not (eq next +eof+))
                       (char= next #\]))
                  (read-chr strm))
                  (return (reverse building)))))

(defun extract-hash (strm chr)
  (declare (type streamable strm)
           (type streamed chr)
           (ignore chr))
  (read-chr strm)
  (loop with building = nil
        with found-sep = t
        with last-read = (extract-list-sep strm (peek-chr strm))
        with next = (peek-chr strm)
        while (and
                (not (eq next +eof+))
                (not (char= next #\})))
        do
        (unless found-sep
          (error 'nrdl-error
            "Expected separating whitespace found at character `~A`."
            next))
        (push (extract-value strm next) building)
        (setf last-read (extract-list-sep strm (peek-chr strm)))
        (setf found-sep (guarded-sepchar-p last-read))
        (setf next (peek-chr strm))
        (unless found-sep
          (error 'nrdl-error
            (concatenate
              'string
              "While looking for val in plist, failed to find separator "
              "whitespace at character `~A`")
            next))
        (push (extract-value strm next) building)
        (setf last-read (extract-list-sep strm (peek-chr strm)))
        (setf found-sep (guarded-sepchar-p last-read))
        (setf next (peek-chr strm))
        finally (when (and
                        (not (eq next +eof+))
                        (char= next #\}))
                  (read-chr strm))
                  (return
                  (alexandria:plist-hash-table
                  (reverse building)
                  :test #'equal))))
#|
(nrdl:parse-from t)
# What now brown cow
{

   the-wind "bullseye"
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

   `force push` >I sing
                >because
                >I'm happy
                ^

   "i am mordac" true
   "I am web mistress ming" false
   "you are so wrong" null
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
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
            ]
}


(parse-from t)
# do
# comments
# work



|#
(defun parse-from (strm)
  (declare (type streamable strm))
  (extract-list-sep strm (peek-chr strm))
  (extract-value strm (peek-chr strm)))



#|
(unprintable-p (code-char #x81))
|#
(defun unprintable-p (chr)
  (declare (type streamed chr))
  (and
    (not (whitespace-p chr))
    (let ((code (char-code chr)))
      (or
        (< code #x1f)
        (= code #x7f)
        (and
          (>= code #x80)
          (<= code #x9f))
        (and
          (>= code #xd800)
          (<= code #xdfff))
        (= code #xfeff)))))

(defun bmp-p (chr)
  (declare (type streamed chr))
  (< (char-code chr) #x10000))

(defun to-surrogates (chr)
  (declare (type streamed chr))
  (let* ((subject (char-code chr))
      (residue (- subject #x10000))
      (higher (prog1 (write (ash residue -10) :base 16) (terpri)))
      (lower (prog1 (write (logand #x003ff residue) :base 16) (terpri))))
      (list
        (+ #xD800 higher)
        (+ #xDc00 lower))))
#|
(inject-quoted
  t
  '(#\ude6d #\\ #\" #\c #\n #\Newline #\Tab #\Return #\c #\a #\b))
(inject-quoted t #\" "asdf

	\\\"blarg")
=>

* (inject-quoted
      t '(#\ude6d #\\ #\" #\c #\n #\Newline #\Tab #\Return #\c #\a #\b))
"\uDE6D\\\"cn\n\t\rcab"
(#\UDE6D #\\ #\" #\c #\n #\Newline #\Tab #\Return #\c #\a #\b)
* (inject-quoted t "asdf
                 ^V
                         \\\"blarg")
"asdf\n\u0016\n\t\\\"blarg"
"asdf
▬
        \\\"blarg"
*



|#

(defparameter *escape-characters*
  '((#\Newline . #\n)
(#\Page . #\f)
(#\Backspace . #\b)
(#\Return . #\r)
(#\Tab . #\t)
(#\\ . #\\)))

(defun inject-quoted (strm blob &optional (quote-char #\"))
  (declare (type streamable strm)
           (type string blob)
           (type character quote-char))
  (write-char quote-char strm)
  (map nil (lambda (c)
             (let ((mapped-char (cdr (assoc c *escape-characters*))))
               (if mapped-char
                 (progn
                   (write-char #\\ strm)
                   (write-char mapped-char strm))
                 (cond
                   ((unprintable-p c)
                    (write-char #\\ strm)
                    (write-char #\u strm)
                    (format strm "~4,'0X" (char-code c)))
                   ((eql c quote-char)
                    (write-char #\\ strm)
                    (write-char quote-char strm))
                   (t (write-char c strm))))))
       blob)
  (write-char quote-char strm)
  blob)
#|

(subseq "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." 0 78)
(break-at-spaces 80 "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
(break-at-spaces 40
"http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_e.htm#element doof")
(break-at-spaces 40
                 "")
(break-up-blob 40
"http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_e.htm#element ")
(break-at-spaces 0 "")

|#
(defun blob-prose-break-spot (max-width blob)
  (declare (type (integer 0 1024) max-width)
           (type string blob))
  (let ((break-spot nil))
    (loop for c across blob
          for pos from 0 to (- (length blob) 1)
          do
          (when (and
                  (char= c #\Space)
                  (or
                    (null break-spot)
                    (< pos max-width)))
            (setf break-spot pos)
            )
            (when (and (>= pos max-width)
                       (not (null break-spot)))
              (return break-spot))
          finally
          (return break-spot))))

(defun blob-verbatim-break-spot (max-width blob)
  (declare (type (integer 0 1024) max-width)
           (type string blob)
           (ignore max-width))
  (position #\Newline blob))

(defun break-up-blob (max-width blob next-spot)
  (declare (type (integer 0 1024) max-width)
           (type string blob)
           (type function next-spot))
  (if (<= (length blob) 0)
    blob
    (loop with consumed = (copy-seq blob)
          with chunks = nil
          for spot = (funcall next-spot max-width consumed)
          while (and
                  (> (length consumed) 0)
                  (not (null spot)))
          do
          (push (subseq consumed 0 spot) chunks)
          (setf consumed
                (if (> (length consumed) (+ 1 spot))
                  (subseq consumed (+ 1 spot))
                  (progn
                    ;; Demonstrate that there should be a trailing newline
                    (push "" chunks)
                    (copy-seq ""))))
          finally
          (progn
            (when (> (length consumed) 0)
              (push consumed chunks))
            (return (reverse chunks))))))

(defun inject-linesep (strm)
  (write-char #\Newline strm))

(defun inject-sep (strm indented-at &key json-mode)
  (declare (type boolean json-mode)
           (type streamable strm)
           (type (or null (integer 0 1024)) indented-at))
  (if (null indented-at)
    (when (not json-mode)
      (write-char #\Space strm))
    (progn
      (inject-linesep strm)
      (loop for i from 1 to indented-at
            do
            (write-char #\Space strm)))))

(defun suggest-line-width
  (indented-at
    &key
    (break-min-width 30)
    (doc-width 80))
  (declare
    (type (or null (integer 0 1024)) indented-at)
    (type (integer 0 1024) break-min-width)
    (type (integer 0 1024) doc-width))
  (when (not (null indented-at))
    (min
      (max
        (- doc-width
           ;; Account for the prefix char
           (+ indented-at 1))
        break-min-width)
      doc-width)))

(defun determine-blob-form (blob line-width json-mode)
  (declare (type string blob)
           (type (or null (integer 0 1024)) line-width))
  (if (or json-mode
          (null line-width))
    :quoted
    (cond
      ((> (count #\Newline blob) 0)
       :verbatim)
      ((and
         (> (length blob) line-width)
         (> (count #\Space blob) 0))
       :prose)
      (t
        :quoted))))

(defun inject-multiline-blob
  (strm blob indented-at line-width prefix-char next-spot)
  (declare (type streamable strm)
           (type string blob)
           (type (integer 0 1024) indented-at)
           (type (integer 0 1024) line-width)
           (type character prefix-char)
           (type function next-spot))
        (loop named line-printer
              with spacious
              for str in (break-up-blob line-width
                                        blob
                                        next-spot)
              do
              (write-char prefix-char strm)
              (write-string str strm)
              (inject-sep strm indented-at))
        (write-char #\^ strm))

(defun inject-blob (strm blob indented-at
                         &key
                         json-mode
                         line-width-args)
  (declare (type streamable strm)
           (type string blob)
           (type (or null (integer 0 1024)) indented-at)
           (type boolean json-mode)
           (type list line-width-args))
  (let* ((line-suggested-width (apply
                                 #'suggest-line-width
                                 (cons indented-at line-width-args)))
         (dispatch (determine-blob-form blob line-suggested-width json-mode)))
    (if (eql dispatch :quoted)
      (inject-quoted
        strm
        blob
        #\")
      (inject-multiline-blob
        strm
        blob
        indented-at
        line-suggested-width
        (if (eql dispatch :verbatim)
          +start-verbatim+
          +start-prose+)
        (if (eql dispatch :verbatim)
                 #'blob-verbatim-break-spot
                 #'blob-prose-break-spot)))))

;; The only place where I punt to the printer
(defun inject-number (strm num)
  (declare (type streamable strm)
           (type number num))
  (prin1 num strm))

(defun escapable-p (chr quote-char)
  (declare (type streamed chr)
           (type character quote-char))
  (or
    (find chr (map 'list #'car *escape-characters*))
    (unprintable-p chr)
    (eql chr quote-char)))

(defun inject-symbol-content (strm prop-content &key json-mode)
  (declare (type string prop-content)
           (type boolean json-mode))
  (if json-mode
    (inject-quoted strm prop-content #\")

    (if (> (count-if (lambda (x)
                       (or
                         (char= x #\Space)
                         (escapable-p x #\`)))
                     prop-content) 0)
      (inject-quoted strm prop-content #\`)
      (write-string prop-content strm))))
#|
(inject-symbol t :argyle)
=> [prints `argyle`]
(inject-symbol t 'terrifying)
=> [throws error, no symbols plz]
(inject-symbol t 15)
=> [throws error, no numbers plz]
(inject-symbol t 't)
=> [prints `true`]
(inject-symbol t :|a b c|)
=> [prints `'a b c'`]

|#
(defun inject-symbol (strm prop &key json-mode)
  (declare (type streamable strm)
           (type (or null boolean symbol) prop)
           (type boolean json-mode))

  (typecase prop
    (null (write-string "false" strm))
    (boolean (write-string "true" strm))
    (keyword (inject-symbol-content
               strm
               (symbol-string prop)
               :json-mode json-mode))
    (symbol (cond ((eql (print prop) 'cl:null)
                        (write-string "null" strm))
                  (t (error
                       'nrdl-error
                       "Writing symbols to NRDL is undefined"))))))

(defun inject-array (strm seq pretty-indent indented-at
                          &key json-mode)
  (let ((array-indent (when (not (null pretty-indent))
                           (+ indented-at pretty-indent))))
    (write-char #\[ strm)
    (loop for r = seq then (cdr r)
          for v = (car r)
          while r
          do
          (inject-sep strm array-indent :json-mode json-mode)
          (inject-value strm v pretty-indent array-indent
                        :json-mode json-mode)
          (when (and (cdr r)
                     json-mode)
            (write-char #\, strm))))
  (inject-sep strm indented-at :json-mode json-mode)
  (write-char #\] strm))

(defun inject-object (strm object pretty-indent indented-at &key json-mode
                           line-width-args)
  (let* ((printable
           (stable-sort
             (alexandria:hash-table-alist object)
             #'string<
             :key
             (lambda (thing)
               (format nil "~A" (car thing)))))
         (object-indent (when (not (null pretty-indent))
                          (+ indented-at pretty-indent))))
    (write-char #\{ strm)
    (loop for r = printable then (cdr r)
          for k = (caar r) and v = (cdar r)
          while r
          do
          (inject-sep strm object-indent :json-mode json-mode)
          (inject-value strm k pretty-indent object-indent
                        :json-mode json-mode)
          (if
              (and
                (typep v 'string)
                (not
                  (eql
                    :quoted
                    (determine-blob-form
                      v
                      (apply
                        #'suggest-line-width
                        (cons
                          (+ object-indent pretty-indent)
                          line-width-args))
                      json-mode))))
              (let ((blob-indent (+ object-indent pretty-indent)))
                (inject-sep strm blob-indent :json-mode json-mode)
                (inject-blob strm v blob-indent :json-mode json-mode))
              (progn
                (if json-mode
                  (progn
                    (write-char #\: strm)
                    (when pretty-indent
                      (write-char #\Space strm)))
                  (write-char #\Space strm))
                (inject-value strm v pretty-indent object-indent :json-mode json-mode)
                (when (and
                        json-mode
                        (cdr r))
                  (write-char #\, strm)))))
    (inject-sep strm indented-at :json-mode json-mode)
    (write-char #\} strm)))

(defun inject-value (strm val pretty-indent indented-at &key json-mode)
  (declare (type streamable strm)
           (type (or null (integer 0 64)) pretty-indent)
           (type (or null (integer 0 1024)) indented-at))
  (typecase val
    ((or null boolean symbol) (inject-symbol strm val :json-mode json-mode))
    (number (inject-number strm val))
    (string (inject-blob strm val indented-at :json-mode json-mode))
    (hash-table (inject-object strm val pretty-indent indented-at :json-mode json-mode))
    (sequence (inject-array strm val pretty-indent indented-at :json-mode json-mode)))
  val)

(defun generate-to (strm val &key (pretty-indent 0) json-mode)
  (declare (type streamable strm)
           (type (or null (integer 0 64)) pretty-indent)
           (type boolean json-mode))
  (inject-value strm val pretty-indent 0 :json-mode json-mode))

(defun nested-to-alist
  (value)
  "
  Recursively changes value, converting all hash tables within the tree to an
  alist.
  "
  (cond
    ((stringp value) value)
    ((or (vectorp value) (listp value))
     (map 'list #'nested-to-alist value))
    ((hash-table-p value)
     (let ((coll
       (loop for k being the hash-key of value
           using (hash-value v)
           collect (cons k (nested-to-alist v)))))
       (stable-sort coll #'string< :key (lambda (thing)
                                          (format nil "~A" (car thing))))))
    (t
      value)))
