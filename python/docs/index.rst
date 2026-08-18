NRDL for Python
===============

``nrdl`` is a pure-Python implementation of
`NRDL <https://github.com/djha-skin/nrdl>`__, the Nestable, Readable
Document Language. NRDL is a superset of JSON that adds:

- **Comments**, starting with ``#`` and running to the end of the line.
- **Symbols**, "program strings" written as barewords (``the-wind``) or
  backtick-quoted (``\`force push\```), useful as object keys.
- **Multi-line strings**: verbatim blocks (``|``-prefixed lines) that
  preserve newlines, and prose blocks (``>``-prefixed lines) whose line
  feeds are folded into single spaces. Blocks end with a caret ``^``.
- **Commas and colons as whitespace**, so ``{a: 1, b: 2}`` and
  ``{a 1 b 2}`` both parse.

Because it is a JSON superset, NRDL can also be used as a plain JSON
parser and serializer (see :doc:`usage`).

Installation
------------

.. code-block:: console

   $ pip install nrdl

Quick start
-----------

.. code-block:: python

   >>> import nrdl
   >>> doc = nrdl.loads('{the-wind "bullseye" the-trees false}')
   {'the-wind': 'bullseye', 'the-trees': False}
   >>> nrdl.dumps(doc, pretty_indent=4)
   '{\\n    the-trees false\\n    the-wind "bullseye"\\n}'

Contents
--------

.. toctree::
   :maxdepth: 2

   usage
   reference
