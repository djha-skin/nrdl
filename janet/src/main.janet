# janet/src/main.janet
#
# Reference Implementation Parser for NRDL in Janet.
#
# This is a port of the Common Lisp reference implementation
# (cl/main.lisp). NRDL is a JSON superset; see the README for the
# language specification and the ABNF that this parser follows.

# --- Character constants ---
#
# Janet strings are byte strings, so characters are byte values.

(def lbrace (in "{" 0))
(def rbrace (in "}" 0))
(def lbracket (in "[" 0))
(def rbracket (in "]" 0))
(def dquote (in "\"" 0))
(def backtick (in "`" 0))
(def pipe (in "|" 0))
(def gt (in ">" 0))
(def hash (in "#" 0))
(def caret (in "^" 0))
(def backslash (in "\\" 0))
(def newline (in "\n" 0))
(def return (in "\r" 0))
(def tab (in "\t" 0))
(def page (in "\f" 0))
(def backspace (in "\b" 0))
(def space (in " " 0))
(def comma (in "," 0))
(def colon (in ":" 0))
(def plus (in "+" 0))
(def minus (in "-" 0))
(def dot (in "." 0))
(def slash (in "/" 0))
(def lt (in "<" 0))
(def bang (in "!" 0))
(def question (in "?" 0))
(def underscore (in "_" 0))
(def equals (in "=" 0))
(def at (in "@" 0))
(def dollar (in "$" 0))
(def percent (in "%" 0))
(def ampersand (in "&" 0))
(def star (in "*" 0))
(def b (in "b" 0))
(def f (in "f" 0))
(def n (in "n" 0))
(def r (in "r" 0))
(def t (in "t" 0))
(def u (in "u" 0))
(def e (in "e" 0))
(def E (in "E" 0))
(def zero (in "0" 0))
(def nine (in "9" 0))
(def a-lower (in "a" 0))
(def z-lower (in "z" 0))
(def A-upper (in "A" 0))
(def Z-upper (in "Z" 0))

# --- Character predicates ---

(defn- digit?
  "True if BYTE is an ASCII digit."
  [byte]
  (<= zero byte nine))

(defn- alpha?
  "True if BYTE is an ASCII letter."
  [byte]
  (or (<= a-lower byte z-lower) (<= A-upper byte Z-upper)))

# --- Unicode ---

(defn- utf8-from-codepoint
  "Encode a Unicode code point as a UTF-8 string."
  [cp]
  (cond
    (< cp 128) (string/from-bytes cp)
    (< cp 2048) (string/from-bytes
                  (+ 192 (math/floor (/ cp 64)))
                  (+ 128 (% cp 64)))
    (< cp 65536) (string/from-bytes
                   (+ 224 (math/floor (/ cp 4096)))
                   (+ 128 (math/floor (% cp 4096) 64))
                   (+ 128 (% cp 64)))
    (string/from-bytes
      (+ 240 (math/floor (/ cp 262144)))
      (+ 128 (math/floor (% cp 262144) 4096))
      (+ 128 (math/floor (% cp 4096) 64))
      (+ 128 (% cp 64)))))

(defn- decode-quoted
  "Decode the escapes in RAW, the content of a quoted string or
  backtick-quoted symbol. Handles \\, \", /, b, f, n, r, t, and
  uXXXX escapes, per the ABNF."
  [raw]
  (def out @[])
  (var i 0)
  (while (< i (length raw))
    (def c (get raw i))
    (if (= c backslash)
      (do
        (set i (+ i 1))
        (def ec (get raw i))
        (cond
          (= ec dquote) (array/push out dquote)
          (= ec backslash) (array/push out backslash)
          (= ec slash) (array/push out slash)
          (= ec b) (array/push out backspace)
          (= ec f) (array/push out page)
          (= ec n) (array/push out newline)
          (= ec r) (array/push out return)
          (= ec t) (array/push out tab)
          (= ec u) (do
                     (def hex (string/slice raw (+ i 1) (+ i 5)))
                     (def cp (scan-number (string "0x" hex)))
                     (unless (and cp (= 4 (length hex)))
                       (error "invalid unicode escape"))
                     (array/concat out (string/bytes (utf8-from-codepoint cp)))
                     (set i (+ i 4)))
          (error (string "invalid escape"))))
      (array/push out c))
    (set i (+ i 1)))
  (string/from-bytes ;out))

(defn- convert-to-symbol
  "Map the literal names true/false/null (and the Common Lisp
  compat literal t/nil) to their values, and everything else to a
  keyword. null becomes the :null keyword, since Janet tables
  cannot hold nil values (the convention used by spork/json)."
  [final-string]
  (cond
    (= final-string "t") true
    (= final-string "nil") :null
    (= final-string "true") true
    (= final-string "false") false
    (= final-string "null") :null
    (keyword final-string)))

# The json-keys-as-keywords option is threaded into the object
# builder through this var; parse-from sets it before matching.
(var *keywordize-keys?* false)

(defn- build-object
  "Build a table from FLAT, a flat array of key/value pairs, turning
  string keys into keywords when keywordization is enabled."
  [flat]
  (def tbl @{})
  (loop [i :range [0 (length flat) 2]]
    (def k (get flat i))
    (def v (get flat (+ i 1)))
    (put tbl (if (and *keywordize-keys?* (string? k)) (keyword k) k) v))
  tbl)

# --- The parser ---
#
# The grammar follows the ABNF in the README. It is expressed with
# Janet's built-in peg module. The reference implementation's
# semantics are preserved: separators are whitespace, commas, and
# colons with interspersed comments; multi-line blobs keep or fold
# their newlines; barewords become keywords.

(def nrdl-grammar
  (peg/compile
    ~{:main (sequence (any :sep) :value (any :sep) -1)
      :sep (some (choice (set " \t\r\n,:") :comment))
      :comment (sequence "#" (any (if (not (set "\r\n")) 1)))
      :value (choice :object :array :quoted-string :quoted-symbol
                     :verbatim :prose :number :bareword)
      :object (cmt (group (sequence "{" (any :sep)
                                    (opt (sequence :value (some :sep) :value))
                                    (any (sequence (some :sep) :value (some :sep) :value))
                                    (any :sep) "}"))
                   ,build-object)
      :array (cmt (group (sequence "[" (any :sep)
                                   (opt (sequence :value (any (sequence (some :sep) :value))))
                                   (any :sep) "]"))
                  ,(fn [items] items))
      :quoted-string (cmt (sequence "\"" (capture (any (choice (sequence "\\" 1)
                                                               (if (not (set "\"")) 1)))) "\"")
                          ,decode-quoted)
      :quoted-symbol (cmt (sequence "`" (capture (any (choice (sequence "\\" 1)
                                                              (if (not (set "`")) 1)))) "`")
                          ,(fn [raw] (convert-to-symbol (decode-quoted raw))))
      :verbatim-line (sequence "|" (capture (any (if (not (set "\r\n")) 1)))
                               (choice "\r\n" "\r" "\n"))
      :verbatim (cmt (sequence :verbatim-line (any (sequence (opt :sep) :verbatim-line))
                               (any :sep) "^")
                     ,(fn [& lines] (string/join lines "\n")))
      :prose-line (sequence ">" (capture (any (if (not (set "\r\n")) 1)))
                            (choice "\r\n" "\r" "\n"))
      :prose (cmt (sequence :prose-line (any (sequence (opt :sep) :prose-line))
                            (any :sep) "^")
                  ,(fn [& lines] (string/join lines " ")))
      :number (cmt (capture (sequence (opt "-")
                                      (choice "0" (sequence (range "19") (any (range "09"))))
                                      (opt (sequence "." (some (range "09"))))
                                      (opt (sequence (set "eE") (opt (set "+-"))
                                                     (some (range "09"))))))
                   ,scan-number)
      :bareword-start (choice (range "az") (range "AZ") (set "_=>@$%&*+/"))
      :bareword-middle (choice :bareword-start (range "09") (set "<!?.-"))
      # Note: use (replace ...) rather than (cmt ...) here, because
      # cmt fails the whole match when its function returns a falsy
      # value, and the bareword `false` legitimately parses to false.
      :bareword (replace (capture (sequence :bareword-start (any :bareword-middle)))
                         ,convert-to-symbol)}))

(defn parse-from
  "Parse a single NRDL value from the string STR and return it.
  Objects become tables, arrays become arrays, symbols become
  keywords, and true/false/null become true/false/nil.
  When :json-keys-as-keywords is true, string keys found in objects
  are converted to keywords, so JSON documents can be parsed into
  tables with keyword keys."
  [str &keys {:json-keys-as-keywords keywordize?}]
  (set *keywordize-keys?* keywordize?)
  (def result (peg/match nrdl-grammar str))
  (unless result (error "parse error"))
  (get result 0))

# --- Generator ---

(defn- unprintable?
  "True if BYTE should be escaped in a quoted string."
  [byte]
  (or (< byte 31)
      (= byte 127)
      (and (>= byte 128) (<= byte 159))
      (and (>= byte 55296) (<= byte 57343))
      (= byte 65279)))

(defn- inject-quoted
  "Write BLOB to BUF as a quote-delimited string, escaping as needed.
  QUOTE-CHAR is the delimiter (double quote for strings, backtick for
  symbols)."
  [buf blob &opt quote-char]
  (def quote-char (or quote-char dquote))
  (buffer/push buf quote-char)
  (each c (string/bytes blob)
    (cond
      (= c newline) (do (buffer/push buf backslash) (buffer/push buf n))
      (= c page) (do (buffer/push buf backslash) (buffer/push buf f))
      (= c backspace) (do (buffer/push buf backslash) (buffer/push buf b))
      (= c return) (do (buffer/push buf backslash) (buffer/push buf r))
      (= c tab) (do (buffer/push buf backslash) (buffer/push buf t))
      (= c backslash) (do (buffer/push buf backslash)
                        (buffer/push buf backslash))
      (unprintable? c) (do (buffer/push buf backslash)
                         (buffer/push buf u)
                         (buffer/format buf "%04x" c))
      (= c quote-char) (do (buffer/push buf backslash)
                         (buffer/push buf quote-char))
      (buffer/push buf c)))
  (buffer/push buf quote-char)
  blob)

(defn- inject-linesep
  "Write a newline to BUF."
  [buf]
  (buffer/push buf newline))

(defn- inject-sep
  "Write a separator. With an indentation level, a newline followed by
  that many spaces; otherwise a single space (unless in json-mode)."
  [buf indented-at &keys {:json-mode json-mode}]
  (if (= nil indented-at)
    (when (not json-mode)
      (buffer/push buf space))
    (do
      (inject-linesep buf)
      (for i 0 indented-at
        (buffer/push buf space)))))

(defn- suggest-line-width
  "Suggest a width for wrapping blobs at the given indentation."
  [indented-at &keys {:break-min-width break-min-width
                      :doc-width doc-width}]
  (default break-min-width 30)
  (default doc-width 80)
  (when (not (= nil indented-at))
    (min (max (- doc-width (+ indented-at 1)) break-min-width)
         doc-width)))

(defn- determine-blob-form
  "Decide how to serialize a string blob: quoted, verbatim, or prose."
  [blob line-width json-mode]
  (if (or json-mode (= nil line-width))
    :quoted
    (cond
      (> (count |(= $ newline) blob) 0) :verbatim
      (and (> (length blob) line-width)
           (> (count |(= $ space) blob) 0)) :prose
      :quoted)))

(defn- blob-prose-break-spot
  "Find the position of the last space within MAX-WIDTH of BLOB."
  [max-width blob]
  (var break-spot nil)
  (for pos 0 (length blob)
    (def c (get blob pos))
    (when (and (= c space) (or (= nil break-spot) (< pos max-width)))
      (set break-spot pos))
    (when (and (>= pos max-width) (not (= nil break-spot)))
      (break)))
  break-spot)

(defn- blob-verbatim-break-spot
  "Find the position of the first newline in BLOB."
  [max-width blob]
  (find-index |(= $ newline) blob))

(defn- break-up-blob
  "Break BLOB into chunks at the spots chosen by NEXT-SPOT."
  [max-width blob next-spot]
  (if (<= (length blob) 0)
    blob
    (do
      (var consumed blob)
      (def chunks @[])
      (var spot (next-spot max-width consumed))
      (while (and (> (length consumed) 0) (not (= nil spot)))
        (array/push chunks (string/slice consumed 0 spot))
        (if (> (length consumed) (+ 1 spot))
          (do
            (set consumed (string/slice consumed (+ 1 spot)))
            (set spot (next-spot max-width consumed)))
          (do
            (array/push chunks "")
            (set consumed "")
            (set spot nil))))
      (when (> (length consumed) 0)
        (array/push chunks consumed))
      chunks)))

(defn- inject-multiline-blob
  "Write BLOB to BUF as a prefixed multi-line string, terminated by
  a caret."
  [buf blob indented-at line-width prefix-char next-spot]
  (each str (break-up-blob line-width blob next-spot)
    (buffer/push buf prefix-char)
    (buffer/push-string buf str)
    (inject-sep buf indented-at))
  (buffer/push buf caret))

(defn- inject-blob
  "Write a string blob to BUF, choosing its form."
  [buf blob indented-at &keys {:json-mode json-mode}]
  (def line-suggested-width (suggest-line-width indented-at))
  (def dispatch (determine-blob-form blob line-suggested-width json-mode))
  (if (= dispatch :quoted)
    (inject-quoted buf blob dquote)
    (inject-multiline-blob buf blob indented-at line-suggested-width
                           (if (= dispatch :verbatim) pipe gt)
                           (if (= dispatch :verbatim)
                             blob-verbatim-break-spot
                             blob-prose-break-spot))))

(defn- inject-number
  "Write a number to BUF."
  [buf num]
  (buffer/push-string buf (string num)))

(defn- escapable?
  "True if BYTE must be escaped inside a backtick-quoted symbol."
  [c quote-char]
  (or (= c newline)
      (= c page)
      (= c backspace)
      (= c return)
      (= c tab)
      (= c backslash)
      (unprintable? c)
      (= c quote-char)))

(defn- inject-symbol-content
  "Write a symbol's name, quoting it when it cannot be a bareword."
  [buf prop-content &keys {:json-mode json-mode}]
  (if json-mode
    (inject-quoted buf prop-content dquote)
    (if (> (count |(or (digit? $) (= $ space) (escapable? $ backtick))
                  prop-content)
           0)
      (inject-quoted buf prop-content backtick)
      (buffer/push-string buf prop-content))))

(defn- inject-symbol
  "Write a boolean, nil, keyword, or the :null sentinel to BUF.
  nil and :null both become null (Janet tables cannot hold nil
  values, so parsed nulls arrive as :null, per spork/json)."
  [buf prop &keys {:json-mode json-mode}]
  (cond
    (nil? prop) (buffer/push-string buf "null")
    (boolean? prop) (buffer/push-string buf (if prop "true" "false"))
    (= prop :null) (buffer/push-string buf "null")
    (keyword? prop) (inject-symbol-content buf (string prop)
                                           :json-mode json-mode)
    (error "Writing symbols to NRDL is undefined")))

# inject-value and inject-array/inject-object are mutually
# recursive, so declare inject-value as a var and set it below.
(var inject-value nil)

(defn- inject-array
  "Write a sequence to BUF as an NRDL array."
  [buf seq pretty-indent indented-at &keys {:json-mode json-mode}]
  (def array-indent (when (not (= nil pretty-indent))
                      (+ indented-at pretty-indent)))
  (buffer/push buf lbracket)
  (for i 0 (length seq)
    (inject-sep buf array-indent :json-mode json-mode)
    (inject-value buf (get seq i) pretty-indent array-indent
                  :json-mode json-mode)
    (when (and json-mode (< i (- (length seq) 1)))
      (buffer/push buf comma)))
  (inject-sep buf indented-at :json-mode json-mode)
  (buffer/push buf rbracket))

(defn- inject-object
  "Write a table or struct to BUF as an NRDL object."
  [buf object pretty-indent indented-at &keys {:json-mode json-mode}]
  (def printable (sort (pairs object)
                       (fn [a b]
                         (< (string (a 0)) (string (b 0))))))
  (def object-indent (when (not (= nil pretty-indent))
                       (+ indented-at pretty-indent)))
  (buffer/push buf lbrace)
  (for i 0 (length printable)
    (def k (get (get printable i) 0))
    (def v (get (get printable i) 1))
    (inject-sep buf object-indent :json-mode json-mode)
    (inject-value buf k pretty-indent object-indent :json-mode json-mode)
    (if (and (string? v)
             # The reference implementation errors here with
             # :pretty-indent nil; guard it so this case degrades to
             # a quoted blob instead.
             (not (= nil pretty-indent))
             (not (= :quoted
                     (determine-blob-form
                       v
                       (suggest-line-width (+ object-indent
                                              pretty-indent))
                       json-mode))))
      (do
        (def blob-indent (+ object-indent pretty-indent))
        (inject-sep buf blob-indent :json-mode json-mode)
        (inject-blob buf v blob-indent :json-mode json-mode))
      (do
        (if json-mode
          (do
            (buffer/push buf colon)
            (when (not (= nil pretty-indent))
              (buffer/push buf space)))
          (buffer/push buf space))
        (inject-value buf v pretty-indent object-indent
                      :json-mode json-mode)
        (when (and json-mode (< i (- (length printable) 1)))
          (buffer/push buf comma)))))
  (inject-sep buf indented-at :json-mode json-mode)
  (buffer/push buf rbrace))

# Write any value to BUF as NRDL, dispatching on its type.
(set inject-value
     (fn inject-value [buf val pretty-indent indented-at
                       &keys {:json-mode json-mode}]
       (cond
         (or (nil? val) (boolean? val) (keyword? val))
         (inject-symbol buf val :json-mode json-mode)
         (number? val) (inject-number buf val)
         (string? val) (inject-blob buf val indented-at
                                    :json-mode json-mode)
         (or (table? val) (struct? val))
         (inject-object buf val pretty-indent indented-at
                        :json-mode json-mode)
         (or (array? val) (tuple? val))
         (inject-array buf val pretty-indent indented-at
                       :json-mode json-mode)
         (error (string/format "Cannot serialize %q to NRDL" val)))))

(defn generate-to
  "Serialize VAL to an NRDL document and return it as a string.
  Tables and structs become objects, arrays and tuples become
  arrays, keywords become symbols, nil becomes null, and true/false
  become true/false.
  :pretty-indent is the number of spaces per indentation level
  (0 means no indentation). :json-mode requests valid JSON output."
  [val &keys {:pretty-indent pretty-indent :json-mode json-mode}]
  (default pretty-indent 0)
  (def buf @"")
  (inject-value buf val pretty-indent 0 :json-mode json-mode)
  (string buf))
