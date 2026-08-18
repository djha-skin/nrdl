Usage
=====

Deserializing: ``parse_from``
-----------------------------

``nrdl.parse_from(text)`` reads one NRDL value from a string and returns
it as Python data:

.. code-block:: python

   >>> import nrdl
   >>> nrdl.parse_from("15")
   15
   >>> nrdl.parse_from('[1 "two" three true null]')
   [1, 'two', 'three', True, None]
   >>> nrdl.parse_from('{a {b [1 2 3]}}')
   {'a': {'b': [1, 2, 3]}}

The mapping between NRDL values and Python values is:

===============  ==============================
NRDL             Python
===============  ==============================
``{ ... }``      ``dict``
``[ ... ]``      ``list``
numbers          ``int`` or ``float``
``"string"``     ``str``
``|...|^``       ``str`` (newlines preserved)
``>...>^``       ``str`` (newlines folded)
symbols          ``str``
``true``         ``True``
``false``        ``False``
``null``         ``None``
===============  ==============================

Symbols parse to plain strings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NRDL symbols -- barewords like ``the-wind`` and backtick-quoted symbols
like ``\`force push\``` -- are "program strings": strings that name
things rather than carry user data. Python has no symbol or keyword
type, so, following the NRDL specification, they deserialize to plain
strings:

.. code-block:: python

   >>> nrdl.parse_from("the-wind")
   'the-wind'
   >>> nrdl.parse_from("\`force push\`")
   'force push'

The three JSON literals are the exception: the symbols ``true``,
``false``, and ``null`` (bareword or backtick-quoted) deserialize to
``True``, ``False``, and ``None``, for JSON backwards compatibility.

Serializing: ``generate_to``
----------------------------

``nrdl.generate_to(value)`` serializes a Python value into an NRDL
document and returns it as a string. Dictionaries, lists, tuples,
strings, numbers, booleans, and ``None`` are supported.

Object keys are written as symbols
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a dictionary is serialized, its **keys are written as symbols** --
this is NRDL's natural way of writing object keys, and it is what keeps
documents readable:

.. code-block:: python

   >>> nrdl.generate_to({"the-wind": "bullseye"})
   '{the-wind "bullseye"}'

A key that cannot be a bareword (for example, one containing spaces) is
written as a backtick-quoted symbol:

.. code-block:: python

   >>> nrdl.generate_to({"force push": "I sing"})
   '{\`force push\` "I sing"}'

Keys that would not survive the trip as symbols -- the empty string and
the literals ``true``/``false``/``null`` -- are written as quoted
strings instead:

.. code-block:: python

   >>> nrdl.generate_to({"true": 1, "": 2})
   '{"true" 1, "" 2}'

String *values*, on the other hand, are always written as quoted
strings, since there is no way in Python to mark a value as a symbol:

.. code-block:: python

   >>> nrdl.generate_to({"his-eye": "a fire"})
   '{his-eye "a fire"}'

Pretty printing
~~~~~~~~~~~~~~~

Pass ``pretty_indent`` to indent nested structures:

.. code-block:: python

   >>> nrdl.generate_to({"a": {"b": 1}}, pretty_indent=4)
   '{\\n    a {\\n        b 1\\n    }\\n}'

The default ``pretty_indent=0`` produces a compact document.

Multi-line strings
~~~~~~~~~~~~~~~~~~

When pretty-printing, long strings are written using NRDL's multi-line
string forms instead of quoted strings:

- A string containing **newlines** is written as a **verbatim** block
  (``|``-prefixed lines whose newlines are preserved), and
- a string that is **too long for its line and contains spaces** is
  written as a **prose** block (``>``-prefixed lines whose line feeds
  fold into single spaces).

Both forms end with a caret ``^``:

.. code-block:: python

   >>> nrdl.generate_to({"poem": "His eye\nis on"}, pretty_indent=4)
   '{\n    poem\n        |His eye\n        |is on\n        ^\n}'
   >>> nrdl.generate_to("x" * 40 + " " + "y" * 40, pretty_indent=4)
   '>xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n>yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy\n^'

Because NRDL is a JSON superset, the block forms parse right back into
the original strings, so round trips are unaffected. Minified output
(``pretty_indent=0``) and ``json_mode`` always use quoted strings.

JSON mode
~~~~~~~~~

Pass ``json_mode=True`` to emit a valid JSON document instead: keys
become quoted strings, ``:`` separates keys from values, and ``,``
separates items:

.. code-block:: python

   >>> nrdl.generate_to({"a": 1, "b": [1, 2]}, json_mode=True)
   '{"a":1,"b":[1,2]}'

Because NRDL is a JSON superset, ``parse_from`` can parse JSON documents
directly, so ``generate_to(..., json_mode=True)`` plus ``parse_from``
round-trips JSON.

Command line
------------

The package installs a ``nrdl`` command that reads NRDL documents and
prints the parsed data -- as NRDL by default, or as JSON with
``--json``:

.. code-block:: console

   $ nrdl config.nrdl
   {
       the-wind "bullseye"
   }
   $ nrdl --json config.nrdl
   {
       "the-wind": "bullseye"
   }
   $ nrdl --indent 0 config.nrdl
   {the-wind "bullseye"}
   $ cat config.nrdl | nrdl
   ...

Documents are read from the named files (multiple files are parsed in
order), or from standard input when no file is given (or the file is
``-``). ``--indent N`` sets the indentation width (default 4; ``0``
emits a compact document), and ``--version`` prints the installed
version.

Round trips
-----------

NRDL documents can be parsed, re-serialized, and parsed again without
loss (modulo key order and comments, which are not part of the data):

.. code-block:: python

   >>> value = nrdl.parse_from("""
   ... {
   ...     the-wind "bullseye"
   ...     the-trees false
   ...     wendover [1 2 3]
   ... }
   ... """)
   >>> value == nrdl.parse_from(nrdl.generate_to(value))
   True
