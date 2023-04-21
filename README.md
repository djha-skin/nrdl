# NeRDL: NEstable, Readable Document Language

This repository houses the nestable readable document language.

## Example Document

## Rationale

This is a language I made up because I am writing a [command line tool
framework][1] and I need a language for it to both read configuration from and
use also to write its output to standard output.

Normally, YAML would be perfect for this; however, in Common Lisp, YAML means
using libyaml and the CFFI, which I wish to avoid. Instead, I wrote this
language myself.

I needed a language that has nestable documents and also have the language be
pleasing to the eye, like you can do in YAML. However, I didn't want to spend
too much time writing a document parser and printer. YAML is [very
complicated][2]. I didn't need a lot of its features.

So I wrote NERDL. Nestable, because you can have nestable documents in it.
Readable, because it's easy both to parse mechanically and is (or can be)
pleasing to the eye.

## Overview

NRDL is (mostly) a superset of JSON  (but duplicate keys are not allowed).

There are specific additions to JSON that NRDL makes that are worth pointing
out.

### Whitespace as a Delimiter (but not a structure signal)

To please the eye, NRDL uses whitespace as a separator, but not as a signaller
of structure. The presence of whitespace is used to separate keys and values,
but indentation doesn't mean anything. For backwards compatibility with JSON,
the colon `:` and comma `,` are considered whitespace characters. braces and
brackets are still used to delimit arrays and objects. Objects must have an
even-numbered number of values in it, with every other value being keys and
their complements being values. Keys can be any type.

### Comments

Comments exist. We need these. They start with a hash symbol `#` and end with
the end of the line.

### Multi-Line Strings

There are two types of multi-line strings in NRDL.

#### Verbatim

The first is a verbatim multi-line string. This string is created by first
taking a normal multiline string, and prefixing each line with an arbitrary
amount of whitespace and the pipe character `|`. The string ends with a line
that only has whitespace followed by the caret `^`.

For example, this multi-line string:


    Once upon a midnight dreary
    While I stumbled, weak and weary

Would become this:

    |Once upon a midnight dreary
    |While I stumbled, weak and weary
    ^

Note that each newline in the string needs its own pipe in order to be counted.
So, this string:

    "a\nb\nc\n"

Would need to be encoded like this, if pipe encoding were desired:

    |a
    |b
    |c
    |
    ^

#### Prose

Prose multi-line strings are exactly like verbatim strings, except that their
line feed characters are replaced by a single space character.

So,

    >a
    >b
    >c
    ^

Becomes

    "a b c"

### Properties

Properties are another type of string introduced into NRDL in hopes of easing
parsing and loading serialization into data structures without adding too much
overhead or complexity.

They are "program strings", strings intended as bookkeeping help to the loading
program, but not really intended to store user data.

Consider the following JSON structure:

    {
        "name": "Daniel Haskin",
        "address": "<REDACTED>"
    }

The strings `"name"` and `"address"` don't actually have anything to do with the
data they are hauling. They are really structure markers. In C and golang, they
are elided entirely, and are simply used to ensure that `"Daniel Haskin"` finds
its way into the `Name` member of a struct.

NRDL has its own data type for this use case. It is a property. This is a string
which can be encoded in one of two ways. The first is using single quotes, `'`,
like this:

    'name'
    'address'

The other is as barewords. The rules around bare words is that they must start
with letters that ensure they don't get confused with numbers. You can't start
with a dash or a dot for example. But you can start with pretty much anything
else:

    name
    address
    __dunder_address__

These are properties.

They are strings which are names of things.

In languages where keywords are a thing, they are encouraged to be decoded as
keywords. This includes Common Lisp, Clojure, Janet, etc. Lots of the lisps.
Also, Randomly, Ruby.

In languages where keywords aren't a data type but symbols are (e.g. the Scheme
Language family), they are encouraged to be decoded as symbols.

In languages closer to the metal, such as C, C++, Golang, Rust, Zig, Etc., they
are encouraged to be used as part of the parsing process, but not really
deserialized per se. In these languages, the property represents literal program
symbols, as in "symbol table" symbols.

Other languages, such as Python, can simply deserialize them as strings (Except
for the barewords `true`, `false`, and `null`, see below), without problems or
repercussions. Properties can be viewed as a specialization of the string type.

They can be used as keys or values.

#### true, false, null

The barewords `true`, `false`, and `null` are NRDL properties. For
JSON compatibility and to obey the law of least surprise, these bare words in
particular are defined, reserved, and are encouraged to be deserialized as
boolean values and/or the nil value, where possible.

These values are good examples of properties used as values and how to deal with
them. In C, the value for `true` is TRUE, which is a program symbol (though not
in the symbol table, _thanks preprocessor_). Deserializing other property values
should be similar; for example, they may be deserialized as enum values.

## ABNF

```
NRDL-text =
             *( ws
                *comment )
             value
value = true
      / false
      / null
      / number
      / string
      / array
      / object

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
zero = %x30             ; 0
one = %x31 ; 

nan = zero fslash zero
inf = one fslash zero

; String section

; plists
object = begin-object
         [ [ value-sep ]
           member
           *( value-sep
              member )
           [ value-sep ] ]
         end-object
begin-object = %x7B ws ; {
val-delim = ws
          / %x2C       ; ,
value-sep = 1*( val-delim
                *comment )
member = key
         name-sep
         value
key = symbol
    / string
symbol = identifier
end-delim = ws
           / %x3A )    ; :
name-sep = 1*( name-delim
               *comment )
end-plist = %x7D      ; }

; Arrays
array = begin-array
        *sep
        
        [ [ value-sep ]
          value
          *( value-sep
             value )
          [ value-sep ] ]
        end-array

begin-array = %x5B     ; [
end-array = %x5D       ; ]

; String section

string = keyword
       / blob

blob = quoted-blob
     / prose-blob
     / verbatim-blob

quoted-blob = double-quote
         *char
          double-quote

char = unescaped
     / escape
     / ( %x22           ; ":U+0022
       / %x5C           ; \:U+005C
       / %x2F           ; /:U+002F
       / %x62           ; b:U+0008
       / %x66           ; f:U+000C
       / %x6E           ; n:U+000A
       / %x72           ; r:U+000D
       / %x74           ; t:U+0009
       / %x75 4hex-digits ) ; uXXXX:
                        ;   U+XXXX
unescaped = %x20-21     ; all
          / %x23-5B     ; except
          / %x5D-10FFFF ; " and \

; Prose section

prose-blob = prose-line *( sep prose-line )

prose-line = prose-mark
             line-content
             line-delimiter
prose-mark = query

; Verbatim section

verbatim-blob = verbatim-line *( sep verbatim-line )
verbatim-line = verbatim-mark
                line-content
                line-delimiter
verbatim-mark = bang

keyword = bareword
        / quoted-keyword

quoted-keyword = single-quote
                *kwchar
                 single-quote
kwchar = single-unescaped
     / escape
     / ( %x21           ; '
       / %x5C           ; \:U+005C
       / %x2F           ; /:U+002F
       / %x62           ; b:U+0008
       / %x66           ; f:U+000C
       / %x6E           ; n:U+000A
       / %x72           ; r:U+000D
       / %x74           ; t:U+0009
       / %x75 4hex-digits) ; uXXXX:
                        ;   U+XXXX
escape = %x5C           ; \
single-unescaped = %x20   ; all
          / %x22   ;
          / %x23-5B     ; except
          / %x5D-10FFFF ; ' and \


bareword = bareword-start 
        *( bareword-middle )

bareword-start = %x24-26 ; %$&
              / %x2B ; +
              / %x2F ; /
              / %x3C-3F ; <=>
              / %x40-5A ; @A-Z
              / %x5F ; _
              / %x61-7A ; a-z
              / %x80-10FF ; hand wave

bareword-middle = bareword-start
                / digit
                / comma
                / colon
                / plus
                / minus
                / dot

hex-digit = digit       ; 0-9
          / %x41-46          ; A-F
          / %x61-66        ; a-f

comment = comment-start
          line-content
          line-delimiter

comment-start = semi-colon

line-content = %x09    ; all
             / %x20-5B ; but
             / %x5D-10FFF
                       ; \r and \n
sep = ws
         *( comment
            ws )
     
ws = blank
   / line-delimiter
   / colon
   / comma

line-delimiter = %x0D %x0A
                       ; \r\n
               / %x0A  ; \n
               / %0D   ; \r
; Rules shared between different
; sections
blank = %x20           ; Space
      / %x09           ; \t
      


comma = %x2C
colon = %x3A
semi-colon = %x3B
hash = %x43
dot = %x2E
plus = %x2B
minus = %x2D
oparen = %x28 ; (
cparen = %x29 ; )
double-quote = %x27 ; "
single-quote = %x22 ; '
bang = %x21 ; !
query= %x3F ; ?
fslash = %x2F ; /
pipe = %x7C ; |
bslash = %x5C ; \
digit = %x30-39         ; 0-9
```