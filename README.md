# NeRDL: NEstable, Readable Document Language

This repository houses the nestable readable document language.

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

   'force push' >I sing
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
using libyaml and the CFFI, which I wish to avoid. Instead, I wrote this
language myself.

I needed a language that has nestable documents and also have the language be
pleasing to the eye, like you can do in YAML. However, I didn't want to spend
too much time writing a document parser and printer. YAML is very
complicated. I didn't need a lot of its features.

So I wrote NRDL. Nestable, because you can have nestable documents in it.
Readable, because it's easy both to parse mechanically and is (or can be)
pleasing to the eye.

## Design Tradeoffs

### Advantages

  - Easy to learn for users, with familiar syntax that matches what is used
    elsewhere.
  - Easy enough to read that it is usable as a configuration language
  - Very easy to parse for implementers, requiring only single-character
    lookahead.
  - Documents can be nested in other documents without changing their content or
    adding a bunch of backslashes everywhere, just like YAML.
  - Comments work the same as YAML.
  - Unlike YAML, it is syntactically specified. This allows editors to help with
    formatting and navigation. This is especially helpful when documeents become
    large.
  - Symbols as a data type is introduced, allowing for easier parsing in typed
    languages and keyword/symbol representation in Lisps and others.

### Caveats

  - Many of the same disadvantages carry over from JSON.
  - If a NRDL document is slurped in, it cannot be printed out again and still
    have it be the same as it was before. This is because order of the keys in
    various often lost in the parsing process, as are comments.
  - Care must be taken when desigining APIs because order is not guaranteed in
    objects.
  - Infinity and NaN are not defined.
  - Single quotes and double quotes are both used in the language, but do not
    mean the same thing and are not interchangeable.
  - Prose doesn't compress multiple whitespace characters at the end of the line
    into one character, as YAML does. This is to ease parsing and keep the
    language simple.

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
data they are hauling. They are really just structure markers. In C and Golang,
they are usually elided entirely, and are simply used to ensure that `"Daniel
Haskin"` finds its way into the `Name` member of a struct. Even the string
`"BROWN"` is only one of a few possible values, and need not be encoded using a
string in the actual program.

NRDL has its own data type for this use case. It is a symbol. This is a string
which can be encoded in one of two ways. The first is using single quotes, `'`,
like this:

```
'name'
'address'
```

Being a string that names something, it is an error to have a symbol which is
empty. The expression `''` is not allowed.

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
      - Languages with a concept of a keyword datatype should decode NRDL
        symbols as keywords. This includes such languages as Common Lisp,
        Clojure, Janet, Elixir, and Ruby.
      - Languages with the concept of a symbol as a datatype but not keywords
        should decode NRDL symbols as symbols. This includes members of the
        Scheme family of Lisps.
      - Otherwise, if these concepts are unavailable or un-idiomatic, they can
        simply be decoded into normal strings without consequence. This category
        includes Python, Perl, PHP, etc.

Note a caveat of this design decision. the literals `'true'` and `true` are
equivalent, and should be treated the same as each other. The same goes for
`'false'` and `false`, and `'null'` and `null`.

Symbols can be used as keys or values, but they are especially useful as object
keys.

## Supporting Resources

Check out the [vim NRDL plugin](https://git.djha.skin/me/vim-nrdl.git).

## Contact Me

I am `skin` on the `#lisp` IRC channel on libera.chat and elsewhere. I will
maintain an IRC channel on that server, `#nrdl`, where people can come and ask
questions.

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
     / ( %x22               ; ":U+0022
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

symchar = single-unescaped
     / escape ( %x21              ; '
              / %x5C              ; \:U+005C
              / %x2F              ; /:U+002F
              / %x62              ; b:U+0008
              / %x66              ; f:U+000C
              / %x6E              ; n:U+000A
              / %x72              ; r:U+000D
              / %x74              ; t:U+0009
              / %x75 4hex-digits) ; uXXXX: U+XXXX

escape = %x5C             ; \
single-unescaped = %x20   ; all
          / %x22          ;
          / %x23-5B       ; except
          / %x5D-10FFFF   ; ' and \


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
                / comma
                / colon
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
             / %x20-5B     ; but
             / %x5D-10FFF  ; \r and \n

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
fslash = %x2F              ; /
pipe = %x7C                ; |
digit = %x30-39            ; 0-9
greater-than = %x3E        ; >
caret = %x5E               ; ^
```