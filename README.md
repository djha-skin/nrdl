# NeRDL: NEstable, Readable Document Language

This repository houses the Nestable Readable Document Language. It is a JSON
superset that was specifically written to be:

- Simple to implement.
- Easy to read.
- Good enough to replace YAML.
- Generic enough to be useful from any programming language.
- Featureful enough to support the functional languages, particularly those in
  the Lisp family.

**Join the [Matrix channel (`nrdl:matrix.org`)](https://matrix.to/#/!mEdAmGzxTrQPWAYdfx:matrix.org?via=matrix.org)!**

## Example Document

```nrdl
# What now brown cow
{

   the-wind "bullseye"
   the-trees false
   the-sparrows his-eye
   poem
    # I don't know if you can hear me
      |His eye
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
   "I am sysadmin" true
   "I am webadmin" false
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

        >Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
        >eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim
        >ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
        >aliquip ex ea commodo consequat. Duis aute irure dolor in
        >reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
        >pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
        >culpa qui officia deserunt mollit anim id est laborum."
        ^
      ]
}

```
## Rationale

This is a language I made up because I am writing a command line tool
framework and I need a language for it to both read configuration from and
use also to write its output to standard output.

Normally, YAML would be perfect for this; however, in Common Lisp, YAML means
using libyaml and the CFFI, which I wish to avoid.

I needed a language that has nestable documents and also have the language be
pleasing to the eye, like is often done with YAML. However, I didn't want to
spend too much time writing a document parser and printer. YAML is very
complicated. I didn't need a lot of its features.

So I wrote NRDL. Nestable, because you can have nestable documents in it.
Readable, because it's easy both to parse mechanically and is (or can be)
pleasing to the eye.

## Overview

NRDL is a superset of JSON.

There are specific additions to JSON that NRDL makes that are worth pointing
out.

### Whitespace as a Delimiter (but not a structure signal)

To please the eye, NRDL uses whitespace as a separator. However, it does not
use it as a signal of structure. The presence of whitespace is used to separate
keys and values, but indentation doesn't mean anything. For backwards
compatibility with JSON, the colon `:` and comma `,` are considered whitespace
characters outside of strings. Braces and brackets are still used to delimit
arrays and objects. Objects must have an even-numbered number of values in it,
with every other value being keys and their complements being values. Keys can
be any type.

### Comments

Comments exist. We need these. They start with a hash symbol `#` and end with
the end of the line.

### Multi-Line Strings

There are two types of multi-line strings in NRDL.

#### Verbatim

The first is a verbatim multi-line string. This string is created by first
taking a normal multiline string, and prefixing each line with an arbitrary
amount of whitespace and the pipe character `|`. The string ends with a line
that only has arbitrary whitespace followed by the caret `^`.

For example, this multi-line string:

```
Once upon a midnight dreary
While I stumbled, weak and weary
```

Would become this:

```
    |Once upon a midnight dreary
    |While I stumbled, weak and weary
    ^
```

Note that each newline in the string needs its own pipe in order to be counted.
So, this string:

```json
    "a\nb\nc\n"
```

Would need to be encoded like this, if pipe encoding were desired:

```nrdl
    |a
    |b
    |c
    |
    ^
```

The normal JSON notation `"a\nb\nc\n"` can still be used if desired.

Comments can be interleaved in between the lines. So, this:

```nrdl
    |a
    # x
    |b
    # y
    |c
    # z
    |
    ^
```

Is the same as the JSON string `"a\nb\nc\n"`.

Note, you CANNOT put comments on the same line as multi-line comments. This
string is equivalent to the JSON string `"a # x\nb # y\nc # z\n"`:

```nrdl
    |a # x
    |b # y
    |c # z
    |
    ^
```

This feature is to enable nested documents within NRDL. Most editors have
block-select and block-edit. You can select multiple lines within a file and
insert `   |` in the front of all of them pretty easily these days. This means
you don't actually have to edit the contents of the document; you can simply
insert a prefix to each line. This operation is easy and easily reversible, and
keeps things readable. These three goals could not be achieved using
backslashes.

#### Prose

Prose multi-line strings are exactly like verbatim strings, except that their
line feed characters are replaced by a single space character.

So,

```nrdl
    >a
    >b
    >c
    ^
```

Is equivalent to the following quoted string:

```nrdl
    "a b c"
```

This is to enable a clean way to print long strings within a document. Editors
have the ability to select a block of text and auto-wrap it into a paragraph,
and can be prefix-aware as well, so that they can make paragraphs in comments.

This same capability of editors can easily be adapted to make it easy to
auto-wrap a long string with a `   >` prefix.

### Symbols

Symbols are another type of string introduced into NRDL in hopes of easing
parsing and loading serialization into data structures without adding too much
overhead or complexity.

They are "program strings", strings intended as bookkeeping help to the loading
program, but not really intended to store user data.

Consider the following JSON structure:

```json
    {
        "name": "Daniel Haskin",
        "address": "<REDACTED>",
        "eye_color": "BROWN"
    }
```

The strings `"name"` and `"address"` don't actually have anything to do with the
data they are hauling. They are really just structure markers. In languages like
C and Golang, they are usually elided entirely, and are simply used to ensure
that `"Daniel Haskin"` finds its way into the `Name` member of a struct. Even
the string `"BROWN"` is only one of a few possible values, and might be encoded
as the name of an enum member in a compiled language.

NRDL has its own data type for this use case. It is a symbol. This is a string
which can be encoded in one of two ways. The first is using backticks,
<code>\`</code>
like this:

```
`name`
`address`
```

Being a string that names something, it is an error to have a symbol which is
empty. The expression <code>``</code> is not allowed.

The other is as barewords. The rules around bare words is that they must start
with characters that ensure they don't get confused with numbers. You can't
start with a dash or a dot for example. But you can start with pretty much
anything else:

    name
    address
    __dunder_address__
    <tag>
    +constant+
    *very-important-concept*
    /materialized/path

These are symbols.

Symbols should be deserialized using
the following guidelines:

  - If the symbol is `true`, `false`, and `null`, these should be decoded into
    the appropriate equivalent concept in the language, as with JSON. This is
    for JSON backwards compatibility and to obey the principle of least
    surprise.
  - Otherwise, they should be decoded into a "special string":
      - Compiled typed languages should attempt to resolve them into actual
        program names, such as struct member names, enumeration values, or names
        of constants. This category includes such languages as C, C++, Java,
        Golang, Rust, and Zig.
      - Languages with a concept of a keyword (a.k.a. atom) datatype should
        decode NRDL symbols as keywords. This includes such languages as Common
        Lisp, Clojure, Janet, Elixir, and Ruby.
      - Languages with the concept of a symbol as a datatype but not keywords
        should decode NRDL symbols as symbols. This includes members of the
        Scheme family of Lisps.
      - Otherwise, if these concepts are unavailable or un-idiomatic, they can
        simply be decoded into normal strings without consequence. This category
        includes Python, Perl, PHP, etc.

Symbols can be used as keys or values, but they are especially useful as object
keys.

## Advantages

  - Easy to learn for users, with familiar syntax that matches what is used
    elsewhere.
  - Easy enough to read that it is usable as a configuration language.
  - Very easy to parse for implementers, requiring only single-character
    lookahead.
  - Documents can be nested in other documents without changing their content or
    adding a bunch of backslashes everywhere, just like YAML.
  - Comments work the same as YAML.
  - Unlike YAML, it is syntactically specified. This allows editors to help with
    formatting and navigation. This is especially helpful when documents become
    large.
  - Symbols as a data type is introduced, allowing for easier parsing in typed
    languages and keyword/symbol representation in Lisps and others.

## Caveats

  - Many of the same disadvantages carry over from JSON:
    - If a NRDL document is deserialized, it cannot be serialized again and still
      have it be the same byte-for-byte as it was before. This is because order
      of the keys is lost in the parsing process, as are comments.
    - Care must be taken when desigining APIs because order is not guaranteed in
      objects.
    - Infinity and NaN are not defined.
  - Prose doesn't compress multiple whitespace characters at the end of the line
    into one character, as YAML does. This is to ease parsing and keep the
    language simple.
  - The literals <code>\`true\`</code> and `true` are equivalent, and should be
    treated the same as each other. The same goes for <code>\`false\`</code> and
    `false`, and <code>\`null\`</code> and `null`. This is, again, in service of
    ensuring that implementing a parser is simple.
  - Adding a caret (`^`) at the end of multi-line strings is important in order
    to ensure that NRDL is an [_LL(1)_
    language](https://en.wikipedia.org/wiki/LL_parser), ensuring implementation
    simplicity. It can also be mildly annoying though.


## Acknowledgements and Comparisons

This format was the result of a lot of research into how other serialization
languages solved the same problems which I wished to solve with NRDL. These
earlier languages had serious advantages and represented rock-solid design
decisions. They also have shortcomings, which is why NRDL was written.

### JSON

From hindsight, JSON seems obvious -- or horrifying -- depending on who is
asked. Having delved the depths of serialization, and beholding its horrors, I
am in awe of the [JSON standard](https://datatracker.ietf.org/doc/html/rfc8259).
It strikes all the right compromises, particularly when it comes to number
serialization. Its syntax is super familiar too. To a first approximation, all
developers know what a curly brace and a square bracket means.

Its only shortcoming is that it is too simple to be used as a configuration file
language. It has no comments and no verbatim multi-line strings. These are the
"killer features" of YAML, and why YAML has taken over the configuration file
space to date, despite its quirks.

Finally, it doesn't support keywords. Using a serialization language that
doesn't support them from a programming language that does causes friction,
while using a serialization language that does support them from a programming
language that doesn't feels just fine.

NRDL addresses these shortcomings with its verbatim multi-line strings, hash
line comments, and support for keywords via its symbols, while largely
preserving its simple, ubiquitous syntax.

### YAML

The [YAML creators](https://yaml.org/) made something really special. Theirs was
the first language I used which allowed me to just indent a JSON blob and have
that document able to be passed as a verbatim string to other programs using its
pipe multi-line strings. Kubernetes makes heavy use of this, and I can't imagine
solving these kinds of ops problems without a language that made document
embedding that easy. They also made a language that, in the wild, looks really
nice, almost like I was reading a simple plain-text email.

The only real shortcoming of YAML is complexity. The language is too complex to
implement easily.  Many languages either simply import PyYAML or libyaml,
both of which were implemented by the designers of YAML themselves, or they
implement [a subset of YAML](https://github.com/Carleslc/Simple-YAML). They do
this because it is very hard to implement the full parser.

They can get away with this because almost no one uses all of YAML's features.
Type tags are both useless and confusing. Typed languages don't need them; the
type of the input values are generally known ahead of time upon deserialization.
Untyped language parsers don't need them either. They can generally deduce the
type based on JSON field kind.

I have seen anchors and refs in the wild, but they generally just cause
confusion and headache. They are too complicated to reason about and don't
really solve the problem they were intended to solve.

Many other minefields and problems exist because of YAML's complexity. The
[Norway problem](https://hitchdev.com/strictyaml/why/implicit-typing-removed/),
the [Billion Laughs](https://en.wikipedia.org/wiki/Billion_laughs_attack)
attack, [the 63 different kinds of multi-line
strings](https://stackoverflow.com/a/21699210). Indeed, NRDL largely agrees with
[StrictYAML's critiques of
YAML](https://hitchdev.com/strictyaml/why-not/ordinary-yaml/), with the
exception that NRDL recognizes the value of providing _some_ implicit typing, at
least insofar as that provided by JSON's types and literals.

Whitespace as a structure marker is also a huge problem. [Tabs are
forbidden](https://yaml.org/faq.html), but are indistinguishable from spaces.
Indentation level works to a point, but if one key contains an embedded
multi-line string and the next key at the same indentation level is off the
page, it's very difficult without additional tooling to tell which indentation
level to which the next key belongs. Some have jokingly resorted to using [a
carpenter's
square](https://salt.tips/text-editor-plugins-for-salt-states-and-yaml-jinja/)
for this problem. This problem crops up all the time in Kubernetes manifests,
for example.

NRDL seeks to preserve the good parts of YAML -- verbatim and prose multi-line
strings, comments, implicit typing to support dynamic languages, pleasing syntax
with emphasis on whitespace, that "text email" feel -- without the bad parts. It
does this by adding _just enough_ syntax to provide YAML's killer features and
nothing else.

This approach also helps NRDL achieve simplicity in parsing. Realizing that the
viability of a technology [depends on its
virality](https://www.dreamsongs.com/RiseOfWorseIsBetter.html), a major goal of
NRDL is to maintain its status as [an _LL(1)_
language](https://en.wikipedia.org/wiki/LL_parser), requiring only
single-character lookahead in the parser implementation. The reference
implementation of NRDL is a simple recursive-descent parser that uses
[`peek-char`](https://www.lispworks.com/documentation/HyperSpec/Body/f_peek_c.htm)
and
[`read-char`](https://www.lispworks.com/documentation/HyperSpec/Body/f_rd_cha.htm).
Most languages have these or similar. This ensures that NRDL can be read without
any buffering, but can deserialized directly. The decision of maintaining _LL(1)_
status is a design decision that, at one stroke, drastically simplifies the
workload of parser implementers. This aims to solve the hampered implementation
proliferation problem that we see with YAML.

### EDN

If NRDL has good looks, it's because it took them in part from
[EDN](https://github.com/edn-format/edn). Its treatment of commas and colons as
whitespace was, in particular, stolen outright. So also was the use of
whitespace as delimiters, but not as structure markers. I have a lot of respect
for Rich Hickey's ability to make code and data beautiful.

However, EDN is very Clojure-centric. Both symbols and keywords are supported,
greatly complicating the syntax without providing much usefulness. People rarely
use the symbol feature, opting instead to serialize with keywords. Within
keywords, only one forward slash is allowed. This is in accordance with
Clojure's syntax, but not with other languages in the Lisp family. The `M` and
`N` suffixes for numbers are rather JVM-specific, and pres-suppose both integer
and floating-point types, when many languages only provide support for 64-bit
floats, such as JavaScript and [Janet](https://janet-lang.org). The struct tags
that EDN provides share many of the disadvantages within a configuration file
context which YAML tags have. Finally, the syntax is very unfamiliar to most
developers' eyes. The Lisp community is comfortable with semicolons as comment
prefixes, but this can be jarring from other perspectives. Configuration files
are meant to be used by a wide audience.

NRDL seeks to support the Lisp family in a practical way while also ensuring
that users of NRDL configuration files don't need to write Lisp to understand
the file. It provides a single symbol type, corresponding to atomic strings
commonly found in data within the Lisp family. It also respects the design
trade-off that JSON made to just specify "numbers" and have the target language
figure out what that means. In doing these things, NRDL aims to be useful within
the context of as many programming languages as possible, with special attention
paid to the functional and Lisp languages.

## How to Contribute

Thanks for reading this far! If you are interested in joining the effort, I
would be more than happy for the help. Thanks for your consideration.

Here are some things I'd like to see for NRDL some day:

* A logo. I am no graphic designer. If you make a logo and it looks pretty good,
  I would be happy to add it to the repo and the matrix channel.
* Multiple implementations in multiple languages. I have implemented the Common
  Lisp implementation, but I would like to see one in C, Scheme, Janet, Elixir,
  etc. I'd be happy to add your implementation to this repository as the standard
  if you wish to implement such a parser.
* I don't use emacs, but would welcome and love a contribution for a NRDL emacs
  plugin. The same goes for VS Code.

## Supporting Resources

Check out the [vim NRDL plugin](https://github.com/djha-skin/vim-nrdl.git).

## Community

NRDL has its own [Matrix channel (`nrdl:matrix.org`)](https://matrix.to/#/!mEdAmGzxTrQPWAYdfx:matrix.org?via=matrix.org).
NRDL also has an IRC channel on Libera.Chat, `#nrdl`. The matrix channel is
preferred.

## ABNF

```

NRDL-text = *( sep )
             value

; String section

value = string
      / symbol
      / number
      / array
      / object

object = begin-object
          *sep
          *(value 1*sep value)
          *(1*sep value 1*sep value)
          *sep
          end-object

begin-object = %x7B ws ; {
end-object = %x7D      ; }

array = begin-array
          *sep
          *value
          *(1*sep value)
          *sep
          end-array

begin-array = %x5B     ; [
end-array = %x5D       ; ]


; Number
number = [ minus ]
         int
         [ frac ]
         [ exp ]
decimal-point = %x2E   ; .
digit1-9 = %x31-39     ; 1-9
e = %x65               ; e
  / %x45               ; E
exp = e
      [ minus
      / plus ]
      1*digit
frac = decimal-point
       1*digit
int = zero
    / ( digit1-9
        *digit )
zero = %x30 ; 0
one = %x31  ;

string = quoted-string
     / prose-string
     / verbatim-string

quoted-string = double-quote
              *char
              double-quote

char = unescaped
     / escape
     / ( %x27               ; ":U+0022
       / %x5C               ; \:U+005C
       / %x2F               ; /:U+002F
       / %x62               ; b:U+0008
       / %x66               ; f:U+000C
       / %x6E               ; n:U+000A
       / %x72               ; r:U+000D
       / %x74               ; t:U+0009
       / %x75 4hex-digits ) ; uXXXX:
                            ; U+XXXX
unescaped = %x20-21         ; all
          / %x23-5B         ; except
          / %x5D-10FFFF     ; " and \

; Prose section

prose-string = prose-line *( sep prose-line ) *sep caret

prose-line = prose-mark
             line-content
             line-delimiter
prose-mark = greater-than

; Verbatim section

verbatim-string = verbatim-line *( sep verbatim-line ) *sep caret

verbatim-line = verbatim-mark
                line-content
                line-delimiter

verbatim-mark = pipe

symbol = bareword
        / quoted-symbol

quoted-symbol = single-quote
                1*symchar
                single-quote

symchar = backtick-unescaped
     / escape ( %x60              ; `
              / %x5C              ; \:U+005C
              / %x2F              ; /:U+002F
              / %x62              ; b:U+0008
              / %x66              ; f:U+000C
              / %x6E              ; n:U+000A
              / %x72              ; r:U+000D
              / %x74              ; t:U+0009
              / %x75 4hex-digits) ; uXXXX: U+XXXX

escape = %x5C             ; \

backtick-unescaped = %x20-5B   ; all
          / %x5D-5F       ; except `
          / %x61-10FFFF   ; and \

bareword = bareword-start
        *( bareword-middle )

bareword-start = %x21     ; !
              / %x24-26   ; %$&
              / %x2B      ; +
              / %x2F      ; /
              / %x3C      ; <
              / %x3D      ; =
              / %x3F      ; ?
              / %x40-5A   ; @A-Z
              / %x5F      ; _
              / %x61-7A   ; a-z
              / %x80-10FF ; hand wave

bareword-middle = bareword-start
                / digit
                / plus
                / minus
                / dot

bareword-end = bareword-start
             / digit
             / plus
             / minus
             / dot

comment = comment-start
          line-content
          line-delimiter
comment-start = hash
sep = ws
         *( comment
            ws )
ws = blank
   / line-delimiter
   / colon
   / comma

hex-digit = digit          ; 0-9
          / %x41-46        ; A-F
          / %x61-66        ; a-f

line-content = %x09        ; all
             / %x20-10FFF  ; but \r and \n

line-delimiter = %x0D %x0A ; \r\n
               / %x0A      ; \n
               / %0D       ; \r

blank = %x20               ; Space
      / %x09               ; \t
comma = %x2C               ; ,
colon = %x3A               ; :
hash = %x43                ; #
dot = %x2E                 ; .
plus = %x2B                ; +
minus = %x2D               ; -
double-quote = %x27        ; "
single-quote = %x22        ; '
backtick = %x60            ; `
fslash = %x2F              ; /
pipe = %x7C                ; |
digit = %x30-39            ; 0-9
greater-than = %x3E        ; >
caret = %x5E               ; ^
```