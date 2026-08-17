# janet/tests/src.janet
#
# Unit tests for the NRDL Janet implementation.

(import ../src)

# --- parse-from: numbers ---

(assert (= 15 (src/parse-from "15"))
        "integer parses")

(assert (= 15 (src/parse-from "15.0"))
        "integral float parses as an integer in Janet")

(assert (= -10 (src/parse-from "-10"))
        "negative integer parses")

(assert (= 1.01 (src/parse-from "1.01"))
        "fractional float parses")

(assert (= 100000 (src/parse-from "1e5"))
        "scientific notation parses")

# --- parse-from: booleans and null ---

(assert (= true (src/parse-from "true"))
        "true parses")

(assert (= false (src/parse-from "false"))
        "false parses")

(assert (= :null (src/parse-from "null"))
        "null parses as the :null sentinel, since Janet tables cannot hold nil")

(assert (= true (src/parse-from "t"))
        "bareword t parses as true, matching the reference")

(assert (= :null (src/parse-from "nil"))
        "bareword nil parses as :null, matching the reference")

# --- parse-from: symbols ---

(assert (= :the-wind (src/parse-from "the-wind"))
        "bareword parses as a keyword")

(assert (= :__dunder_address__ (src/parse-from "__dunder_address__"))
        "underscore bareword parses as a keyword")

(assert (= :+constant+ (src/parse-from "+constant+"))
        "plus-delimited bareword parses as a keyword")

(assert (= :/materialized/path (src/parse-from "/materialized/path"))
        "slash bareword parses as a keyword")

(assert (= (keyword "force push") (src/parse-from "`force push`"))
        "backtick-quoted symbol parses as a keyword")

# --- parse-from: strings ---

(assert (= "hello" (src/parse-from "\"hello\""))
        "quoted string parses")

(assert (= "a\nb" (src/parse-from "\"a\\nb\""))
        "newline escape parses")

(assert (= "a\"b" (src/parse-from "\"a\\\"b\""))
        "escaped quote parses")

(assert (= "A" (src/parse-from "\"\\u0041\""))
        "unicode escape parses")

(assert (= "café" (src/parse-from "\"café\""))
        "utf-8 string parses")

# --- parse-from: arrays ---

(assert (deep= @[:a] (src/parse-from "[a]"))
        "single-element array parses")

(assert (deep= @[:a :b :c] (src/parse-from "[a b c]"))
        "space-separated array parses")

(assert (deep= @[1 2 3] (src/parse-from "[1,2,3]"))
        "comma-separated array parses")

(assert (deep= @[] (src/parse-from "[]"))
        "empty array parses")

(assert (deep= @[1 "two" :three] (src/parse-from "[1 \"two\" three]"))
        "heterogeneous array parses")

# --- parse-from: objects ---

(assert (deep= @{} (src/parse-from "{}"))
        "empty object parses")

(assert (deep= @{:a 1 :b 2} (src/parse-from "{a 1 b 2}"))
        "bareword-keyed object parses")

(assert (deep= @{"a" 1 "b" 2} (src/parse-from "{\"a\": 1, \"b\": 2}"))
        "json-style object parses with string keys by default")

(assert (deep= @{"a" @{"b" @[1 2]}} (src/parse-from "{\"a\": {\"b\": [1, 2]}}"))
        "nested json-style structure parses")

(assert (deep= @{1 "one" true "yes" :null "no"} (src/parse-from "{1 \"one\" true \"yes\" null \"no\"}"))
        "non-symbol keys parse")

# --- parse-from: comments and whitespace ---

(assert (= 42 (src/parse-from "# hi\n42"))
        "line comment is skipped")

(assert (deep= @[:a] (src/parse-from "  \n  [a]  "))
        "leading and trailing whitespace is skipped")

(assert (deep= @{:a 1} (src/parse-from "{# a comment\n a 1}"))
        "comment inside object is skipped")

# --- parse-from: multi-line blobs ---

(assert (= "His eye\nis on" (src/parse-from "|His eye\n|is on\n^"))
        "verbatim blob parses")

(assert (= "a\nb\n" (src/parse-from "|a\n|b\n|\n^"))
        "verbatim blob with trailing empty line parses")

(assert (= "I sing because" (src/parse-from ">I sing\n>because\n^"))
        "prose blob parses newlines as spaces")

(assert (= "a\nb\nc" (src/parse-from "|a\n# comment\n|b\n# another\n|c\n^"))
        "comments between verbatim lines are skipped")

# --- parse-from: errors ---

(assert (try (do (src/parse-from "") false) ((e) true))
        "empty input raises an error")

(assert (try (do (src/parse-from "!") false) ((e) true))
        "garbage input raises an error")

(assert (try (do (src/parse-from "[a !]") false) ((e) true))
        "garbage inside an array raises an error")

# --- parse-from: json-keys-as-keywords ---

(assert (deep= @{:zick @{:config-files @["one" "two"]
                         :ghost-files @["three"]}}
               (src/parse-from "{\"zick\": {\"config-files\": [\"one\", \"two\"], \"ghost-files\": [\"three\"]}}"
                                :json-keys-as-keywords true))
        "string keys become keywords when requested")

(assert (deep= @{"zick" @{"config-files" @["one" "two"]
                          "ghost-files" @["three"]}}
               (src/parse-from "{\"zick\": {\"config-files\": [\"one\", \"two\"], \"ghost-files\": [\"three\"]}}"))
        "string keys stay strings by default")

(assert (deep= @[@{:a 1} @{:b 2}]
               (src/parse-from "[{\"a\": 1}, {\"b\": 2}]" :json-keys-as-keywords true))
        "nested maps inside arrays are keywordized")

(assert (deep= @{:a "hello"}
               (src/parse-from "{\"a\": \"hello\"}" :json-keys-as-keywords true))
        "string values are unaffected by keywordization")

(assert (deep= @{:zick 1}
               (src/parse-from "{zick 1}" :json-keys-as-keywords true))
        "bareword keys are keywords either way")

# --- generate-to: scalars ---

(assert (= "null" (src/generate-to nil))
        "nil generates null")

(assert (= "true" (src/generate-to true))
        "true generates true")

(assert (= "false" (src/generate-to false))
        "false generates false")

(assert (= "foo" (src/generate-to :foo))
        "keyword generates a bareword")

(assert (= "\"hello\"" (src/generate-to "hello"))
        "string generates a quoted blob")

(assert (= "15" (src/generate-to 15))
        "number generates a number")

# --- generate-to: structures ---

(assert (= "{\n    a 1\n}" (src/generate-to @{:a 1} :pretty-indent 4))
        "object generates with pretty indentation")

(assert (= "{\n    `a b` 1\n}" (src/generate-to @{(keyword "a b") 1} :pretty-indent 4))
        "keyword with a space generates backtick-quoted")

(assert (= "|a\n|b\n|\n^" (src/generate-to "a\nb\n" :pretty-indent 4))
        "string with newlines generates a verbatim blob")

# --- generate-to: json-mode ---

(assert (= "{\n    \"a\": 1,\n    \"b\": [\n        \"x\",\n        \"y\",\n        \"z\"\n    ],\n    \"c\": [\n        \"food\",\n        \"for\",\n        \"thought\"\n    ],\n    \"d\": null,\n    \"e\": false,\n    \"f\": true,\n    \"g\": 0.87\n}"
           (src/generate-to @{:a 1
                               :b [:x :y :z]
                               :c ["food" "for" "thought"]
                               :d :null
                               :e false
                               :f true
                               :g 0.87}
                             :json-mode true
                             :pretty-indent 4))
        "json-mode generates valid json")

# --- round-trips ---

(def sparrow "\n  # What now brown cow\n  {\n  the-wind \"bullseye\"\n  the-trees false\n  the-sparrows his-eye\n  poem\n  # I don't know if you can hear me\n  |His eyee\n  # or if\n  # you're even there\n  |is on\n  # I don't know if you can listen\n  |The sparrow\n  ^\n\n  # to a gypsy's prayer\n\n  this-should-still-work 15.0\n  other\n  |And I know\n  |He's watching\n  |Over me\n  ^\n\n  `force push`\n  >I sing\n  >because\n  >I'm happy\n  ^\n\n  \"i am mordac\" true\n  \"I am web mistress ming\" false\n  \"you are so wrong\" null\n  wendover [\n  {\n  so 1\n  much -10\n  gambling 100\n  but 1000\n  also -1000\n  apparently 10000\n  paramedics -10000\n  and 1.01\n  }\n  {\n  die in\n  a fire\n  }\n  15\n  |this\n  |that\n  ^\n  \"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\"\n  ]\n  }")

(def sparrow-parsed (src/parse-from sparrow))

(assert (deep= @{:the-wind "bullseye"}
               (src/parse-from "{the-wind \"bullseye\"}"))
        "simple object round-trips through parse")

(assert (deep= sparrow-parsed
               (src/parse-from (src/generate-to sparrow-parsed :pretty-indent 4)))
        "sparrow document round-trips through generate")

# JSON has no symbol type, so keyword values become strings in
# json-mode; keywordize the keys on the way back in. This helper
# applies the same transformation to the original for comparison.
(defn json-ify
  "Keywordize table keys and stringify keyword values, matching what
  json-mode generate + reparse with :json-keys-as-keywords produces."
  [x]
  (cond
    (table? x) (do (def t @{})
                 (each [k v] (pairs x)
                   (put t (keyword k) (json-ify v)))
                 t)
    (array? x) (map json-ify x)
    (and (keyword? x) (not (= x :null))) (string x)
    x))

(assert (deep= (json-ify sparrow-parsed)
               (src/parse-from (src/generate-to sparrow-parsed :json-mode true)
                                :json-keys-as-keywords true))
        "sparrow document round-trips through json-mode generate")
