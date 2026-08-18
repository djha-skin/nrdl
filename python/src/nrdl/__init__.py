"""nrdl -- a Python implementation of NRDL, the Nestable Readable Document Language.

NRDL is a superset of JSON. It adds comments (``#`` to end of line),
multi-line strings (``|`` verbatim and ``>`` prose blocks terminated by
``^``), and symbols (barewords or backtick-quoted program strings).
Commas and colons are treated as whitespace. See the repository README
for the language specification and ABNF.

This module provides two functions:

* :func:`parse_from` -- deserialize an NRDL document into Python data.
* :func:`generate_to` -- serialize Python data into an NRDL document.

Because Python has no symbol type, NRDL symbols deserialize to plain
strings. The three JSON literals ``true``, ``false``, and ``null``
deserialize to ``True``, ``False``, and ``None``. When serializing,
dictionary keys are written as symbols (a bareword when possible,
otherwise backtick-quoted), while string values are written as quoted
strings.
"""

from __future__ import annotations

import re

__all__ = ["NrdlError", "parse_from", "generate_to"]


class NrdlError(ValueError):
    """Raised when a document cannot be parsed as NRDL."""


# The JSON-compatible literals. Per the README, the symbols `true`,
# `false`, and `null` (bareword or backtick-quoted) decode to the
# appropriate Python values.
_LITERALS = {"true": True, "false": False, "null": None}

# Escapes allowed inside quoted strings and backtick-quoted symbols,
# per the ABNF `escape` rule.
_ESCAPES = {
    '"': '"',
    "\\": "\\",
    "/": "/",
    "b": "\b",
    "f": "\f",
    "n": "\n",
    "r": "\r",
    "t": "\t",
    "`": "`",
}

# number = [ minus ] int [ frac ] [ exp ]
_NUMBER_RE = re.compile(r"-?(?:0|[1-9][0-9]*)(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?\Z")
_NUMBER_CHARS = frozenset("0123456789+-.eE")

# bareword-start and bareword-middle, per the README ABNF. Characters
# with code points >= 0x80 are also allowed (the ABNF's "hand wave").
# `*` is not in the ABNF's bareword-start set, but the README lists
# `*very-important-concept*` as an example bareword, so it is allowed.
_BAREWORD_START = frozenset(
    "!$%&*+/<=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"
)
# `>` may appear inside a bareword (the README lists `<tag>` as a
# bareword) but not at its start, since a value beginning with `>` is a
# prose string.
_BAREWORD_MIDDLE = _BAREWORD_START | frozenset("0123456789+->.")


def parse_from(text):
    """Deserialize one NRDL value from ``text`` and return it as Python data.

    Objects become dictionaries, arrays become lists, numbers become
    ``int``/``float``, quoted strings become strings, and symbols become
    plain strings. The symbols ``true``, ``false``, and ``null`` become
    ``True``, ``False``, and ``None``.

    Raises :class:`NrdlError` if ``text`` is not a single valid NRDL value.
    """
    parser = _Parser(text)
    parser.skip_sep()
    value = parser.value()
    parser.skip_sep()
    if not parser.eof():
        raise parser.error("trailing content after value")
    return value


class _Parser:
    """A simple recursive-descent NRDL parser over a string.

    The parser follows the ABNF in the repository README directly,
    consuming one character at a time with single-character lookahead.
    """

    def __init__(self, text):
        self.text = text
        self.pos = 0

    # -- low-level helpers -------------------------------------------

    def eof(self):
        return self.pos >= len(self.text)

    def peek(self):
        return self.text[self.pos] if not self.eof() else None

    def advance(self):
        if self.eof():
            raise self.error("unexpected end of document")
        c = self.text[self.pos]
        self.pos += 1
        return c

    def error(self, message):
        line = self.text.count("\n", 0, self.pos) + 1
        column = self.pos - self.text.rfind("\n", 0, self.pos)
        return NrdlError("%s at line %d, column %d" % (message, line, column))

    @staticmethod
    def is_bareword_start(c):
        return c in _BAREWORD_START or ord(c) >= 0x80

    @staticmethod
    def is_bareword_middle(c):
        return c in _BAREWORD_MIDDLE or ord(c) >= 0x80

    # -- separators --------------------------------------------------

    def skip_sep(self):
        """Skip separators: whitespace, commas, colons, and comments.

        Returns ``True`` if at least one character was consumed.
        """
        start = self.pos
        while not self.eof():
            c = self.text[self.pos]
            if c in " \t\r\n,:":
                self.pos += 1
            elif c == "#":
                while not self.eof() and self.text[self.pos] not in "\r\n":
                    self.pos += 1
            else:
                break
        return self.pos > start

    # -- values ------------------------------------------------------

    def value(self):
        c = self.peek()
        if c is None:
            raise self.error("expected a value")
        if c == "{":
            return self.object()
        if c == "[":
            return self.array()
        if c == '"':
            return self.quoted_string()
        if c == "`":
            return self.quoted_symbol()
        if c == "|":
            return self.multiline("|", "\n")
        if c == ">":
            return self.multiline(">", " ")
        if c == "-" or "0" <= c <= "9":
            return self.number()
        if self.is_bareword_start(c):
            return self.bareword()
        raise self.error("unexpected character %r" % c)

    def object(self):
        """object = begin-object *sep *(value 1*sep value) *sep end-object"""
        self.advance()  # {
        self.skip_sep()
        result = {}
        while True:
            c = self.peek()
            if c is None:
                raise self.error("unterminated object (missing `}`)")
            if c == "}":
                self.advance()
                return result
            key = self.value()
            if not self.skip_sep():
                raise self.error("expected a separator between object key and value")
            result[key] = self.value()
            self.skip_sep()

    def array(self):
        """array = begin-array *sep *value *(1*sep value) *sep end-array"""
        self.advance()  # [
        result = []
        need_sep = False
        while True:
            consumed_sep = self.skip_sep()
            c = self.peek()
            if c is None:
                raise self.error("unterminated array (missing `]`)")
            if c == "]":
                self.advance()
                return result
            if need_sep and not consumed_sep:
                raise self.error("expected a separator between array values")
            result.append(self.value())
            need_sep = True

    def quoted_string(self):
        """string = double-quote *char double-quote"""
        self.advance()  # "
        out = []
        while True:
            c = self.advance()
            if c == '"':
                return "".join(out)
            if c == "\\":
                out.append(self.escape())
            else:
                out.append(c)

    def quoted_symbol(self):
        """symbol = single-quote 1*symchar single-quote (backtick-quoted)."""
        self.advance()  # `
        out = []
        while True:
            c = self.advance()
            if c == "`":
                break
            if c == "\\":
                out.append(self.escape())
            else:
                out.append(c)
        name = "".join(out)
        if not name:
            raise self.error("a backtick-quoted symbol may not be empty")
        return _LITERALS.get(name, name)

    def escape(self):
        """escape = backslash ( " \\ / b f n r t uXXXX )"""
        c = self.advance()
        if c == "u":
            hex_digits = "".join(self.advance() for _ in range(4))
            if not all(d in "0123456789abcdefABCDEF" for d in hex_digits):
                raise self.error("invalid unicode escape \\u%s" % hex_digits)
            return chr(int(hex_digits, 16))
        if c in _ESCAPES:
            return _ESCAPES[c]
        raise self.error("invalid escape \\%s" % c)

    def multiline(self, marker, joiner):
        """verbatim-string / prose-string.

        A sequence of ``marker``-prefixed lines whose content is joined
        with ``joiner`` ("\\n" for verbatim, " " for prose), terminated
        by a caret. Comments and other separators may appear between
        lines.
        """
        lines = []
        while True:
            self.advance()  # the line marker
            content = []
            while not self.eof() and self.peek() not in "\r\n":
                content.append(self.advance())
            lines.append("".join(content))
            # Every line ends with a line delimiter.
            if self.eof():
                raise self.error(
                    "unterminated %s string (missing `^`)" % ("verbatim" if marker == "|" else "prose")
                )
            if self.peek() == "\r":
                self.advance()
                if self.peek() == "\n":
                    self.advance()
            else:
                self.advance()  # \n
            self.skip_sep()
            c = self.peek()
            if c == "^":
                self.advance()
                return joiner.join(lines)
            if c == marker:
                continue
            if c is None:
                raise self.error(
                    "unterminated %s string (missing `^`)" % ("verbatim" if marker == "|" else "prose")
                )
            raise self.error(
                "expected `%s` or `^` in multi-line string, got %r" % (marker, c)
            )

    def number(self):
        """number = [ minus ] int [ frac ] [ exp ]"""
        start = self.pos
        while not self.eof() and self.text[self.pos] in _NUMBER_CHARS:
            self.pos += 1
        token = self.text[start : self.pos]
        if not _NUMBER_RE.match(token):
            raise self.error("invalid number %r" % token)
        if "." in token or "e" in token or "E" in token:
            return float(token)
        return int(token)

    def bareword(self):
        """symbol = bareword-start *( bareword-middle )"""
        out = []
        while not self.eof() and self.is_bareword_middle(self.peek()):
            out.append(self.advance())
        name = "".join(out)
        return _LITERALS.get(name, name)


def generate_to(value, pretty_indent=0, json_mode=False):
    """Serialize ``value`` into an NRDL document and return it as a string.

    ``value`` may be any combination of dictionaries, lists, tuples,
    strings, numbers, booleans, and ``None``. Dictionaries become NRDL
    objects with their keys written as symbols; lists and tuples become
    arrays; strings become quoted strings; ``True``, ``False``, and
    ``None`` become ``true``, ``false``, and ``null``.

    ``pretty_indent`` is the number of spaces used for each level of
    indentation; ``0`` emits a compact document. ``json_mode`` emits a
    valid JSON document instead (string keys, colons, and commas).

    Raises :class:`TypeError` for unsupported value types.
    """
    if pretty_indent < 0:
        raise ValueError("pretty_indent must be >= 0")
    writer = _Writer(pretty_indent, json_mode)
    writer.value(value, 0)
    return "".join(writer.out)


class _Writer:
    """Serializes Python values to NRDL, one piece at a time."""

    def __init__(self, pretty, json_mode):
        self.pretty = pretty
        self.json = json_mode
        self.out = []

    def sep(self, indent):
        """Separator before each item at the given indentation level."""
        if self.pretty:
            self.out.append("\n")
            self.out.append(" " * indent)
        elif not self.json:
            self.out.append(" ")

    def close_sep(self, indent):
        """Separator before a closing bracket/brace."""
        if self.pretty:
            self.out.append("\n")
            self.out.append(" " * indent)

    def value(self, v, indent):
        if v is None:
            self.out.append("null")
        elif v is True:
            self.out.append("true")
        elif v is False:
            self.out.append("false")
        elif isinstance(v, int):
            self.out.append(str(v))
        elif isinstance(v, float):
            if v != v or v in (float("inf"), float("-inf")):
                raise ValueError("NRDL does not define Infinity or NaN")
            self.out.append(repr(v))
        elif isinstance(v, str):
            self.blob(v, indent)
        elif isinstance(v, dict):
            self.object(v, indent)
        elif isinstance(v, (list, tuple)):
            self.array(v, indent)
        else:
            raise TypeError("cannot serialize %s to NRDL" % type(v).__name__)

    def array(self, seq, indent):
        self.out.append("[")
        for i, v in enumerate(seq):
            if self.pretty or i > 0:
                self.sep(indent + self.pretty)
            self.value(v, indent + self.pretty)
            if self.json and i < len(seq) - 1:
                self.out.append(",")
        self.close_sep(indent)
        self.out.append("]")

    def object(self, d, indent):
        self.out.append("{")
        items = sorted(d.items(), key=lambda kv: str(kv[0]))
        for i, (k, v) in enumerate(items):
            if self.pretty or i > 0:
                self.sep(indent + self.pretty)
            self.key(k, indent + self.pretty)
            if isinstance(v, str) and self.pretty and not self.json:
                # A long string is written as a multi-line blob on its
                # own line, indented one level deeper than the key.
                blob_indent = indent + 2 * self.pretty
                if self.blob_form(v, blob_indent) != "quoted":
                    self.sep(blob_indent)
                    self.blob(v, blob_indent)
                    continue
            if self.json:
                self.out.append(":")
                if self.pretty:
                    self.out.append(" ")
            else:
                self.out.append(" ")
            self.value(v, indent + self.pretty)
            if self.json and i < len(items) - 1:
                self.out.append(",")
        self.close_sep(indent)
        self.out.append("}")

    def key(self, k, indent):
        """Serialize a dictionary key.

        In NRDL mode keys are written as symbols -- a bareword when the
        string can be one, otherwise backtick-quoted, and a quoted
        string when neither would deserialize back to the same value
        (empty strings and the literals ``true``/``false``/``null``).
        Non-string keys (numbers, booleans, ``None``) are written as
        their values. In JSON mode keys are always quoted strings.
        """
        if self.json:
            self.quoted(k if isinstance(k, str) else str(k))
        elif isinstance(k, str):
            self.symbol(k)
        elif k is None or k is True or k is False or isinstance(k, (int, float)):
            self.value(k, indent)
        else:
            raise TypeError("cannot serialize %s as an NRDL object key" % type(k).__name__)

    def symbol(self, s):
        """Write a string as an NRDL symbol (an object key)."""
        if not s or s in _LITERALS:
            # An empty symbol is not allowed, and `true`/`false`/`null`
            # would deserialize to booleans/None; fall back to a quoted
            # string so the key round-trips.
            self.quoted(s)
        elif self.can_bareword(s):
            self.out.append(s)
        elif all(ord(c) >= 0x20 for c in s):
            # A backtick-quoted symbol can hold any printable character;
            # backticks and backslashes are escaped.
            self.out.append("`")
            for c in s:
                if c in "`\\":
                    self.out.append("\\")
                self.out.append(c)
            self.out.append("`")
        else:
            self.quoted(s)

    @staticmethod
    def can_bareword(s):
        if not s or not _Parser.is_bareword_start(s[0]):
            return False
        return all(_Parser.is_bareword_middle(c) for c in s)

    def blob_form(self, s, indent):
        """Decide how to serialize a string: quoted, verbatim, or prose.

        Multi-line output is only used when pretty-printing; minified
        and json-mode documents always use quoted strings. A string
        containing newlines becomes a verbatim block, and a string too
        long for its line that contains spaces becomes a prose block.
        """
        if self.json or not self.pretty:
            return "quoted"
        if "\n" in s:
            if "\r" not in s and self._blob_chars_ok(s):
                return "verbatim"
            return "quoted"
        if len(s) > self.line_width(indent) and " " in s and self._blob_chars_ok(s):
            return "prose"
        return "quoted"

    @staticmethod
    def _blob_chars_ok(s):
        """True if every character may appear in a multi-line line."""
        return all(c == "\n" or c == "\t" or ord(c) >= 0x20 for c in s)

    def line_width(self, indent):
        """Suggested width for wrapping a blob at the given indentation."""
        return min(max(80 - (indent + 1), 30), 80)

    def blob(self, s, indent):
        form = self.blob_form(s, indent)
        if form == "verbatim":
            self.verbatim(s, indent)
        elif form == "prose":
            self.prose(s, indent)
        else:
            self.quoted(s)

    def verbatim(self, s, indent):
        """Write a string as a verbatim multi-line block ending in `^`."""
        for line in s.split("\n"):
            self.out.append("|")
            self.out.append(line)
            self.sep(indent)
        self.out.append("^")

    def prose(self, s, indent):
        """Write a long string as a prose block folded at spaces."""
        width = self.line_width(indent)
        for chunk in self._prose_chunks(s, width):
            self.out.append(">")
            self.out.append(chunk)
            self.sep(indent)
        self.out.append("^")

    @staticmethod
    def _prose_chunks(s, width):
        """Split ``s`` at spaces so joining the chunks with a single space
        reconstructs ``s`` exactly (the space at each split point is
        dropped and replaced by the fold space)."""
        chunks = []
        start = 0
        n = len(s)
        while n - start > width:
            window = s[start : start + width + 1]
            spot = window.rfind(" ")
            if spot < 0:
                break  # no space to fold at; keep the rest on one line
            chunks.append(s[start : start + spot])
            start = start + spot + 1
        chunks.append(s[start:])
        return chunks

    def quoted(self, s):
        """Write a string as a double-quoted string with escapes."""
        self.out.append('"')
        for c in s:
            if c == '"':
                self.out.append('\\"')
            elif c == "\\":
                self.out.append("\\\\")
            elif c == "\n":
                self.out.append("\\n")
            elif c == "\r":
                self.out.append("\\r")
            elif c == "\t":
                self.out.append("\\t")
            elif c == "\b":
                self.out.append("\\b")
            elif c == "\f":
                self.out.append("\\f")
            elif ord(c) < 0x20:
                self.out.append("\\u%04x" % ord(c))
            else:
                self.out.append(c)
        self.out.append('"')
