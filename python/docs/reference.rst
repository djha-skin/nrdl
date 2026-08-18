API reference
=============

The API follows the :mod:`json` module's conventions: ``load``/``dump``
work with file handles, while ``loads``/``dumps`` work with strings
(``loads`` feeds an ``io.StringIO`` into ``load``, and ``dumps``
collects ``dump``'s output from an ``io.StringIO``).

.. function:: load(fh)

   Deserialize one NRDL value from the file handle ``fh`` and return it
   as Python data.

   ``fh`` must be a text-mode file-like object with a ``read`` method,
   for example the result of ``open(path, encoding="utf-8")`` or an
   ``io.StringIO``. The whole document is read and must contain a single
   NRDL value.

   Objects become dictionaries, arrays become lists, numbers become
   ``int``/``float``, quoted strings become strings, and symbols become
   plain strings. The symbols ``true``, ``false``, and ``null`` become
   ``True``, ``False``, and ``None``.

   :param fh: a text-mode file-like object to read from
   :raises NrdlError: if the document is not a single valid NRDL value

.. function:: loads(text)

   Deserialize one NRDL value from the string ``text`` and return it as
   Python data.

   Equivalent to ``load(io.StringIO(text))``; see :func:`load`.

   :param text: an NRDL document containing a single value
   :type text: str
   :raises NrdlError: if ``text`` is not a single valid NRDL value

.. function:: dump(value, fh, pretty_indent=0, json_mode=False)

   Serialize ``value`` into an NRDL document, writing it to the file
   handle ``fh``.

   ``value`` may be any combination of dictionaries, lists, tuples,
   strings, numbers, booleans, and ``None``. Dictionaries become NRDL
   objects with their **keys written as symbols**; lists and tuples
   become arrays; strings become quoted strings; ``True``, ``False``,
   and ``None`` become ``true``, ``false``, and ``null``.

   ``fh`` must be a text-mode file-like object with a ``write`` method,
   for example the result of ``open(path, "w", encoding="utf-8")`` or
   an ``io.StringIO``.

   :param value: the value to serialize
   :param fh: a text-mode file-like object to write to
   :param pretty_indent: spaces per indentation level; ``0`` (the
       default) produces a compact document
   :type pretty_indent: int
   :param json_mode: if true, emit a valid JSON document (string keys,
       colons, and commas)
   :type json_mode: bool
   :returns: ``None``
   :raises TypeError: if ``value`` contains an unsupported type
   :raises ValueError: if ``pretty_indent`` is negative or ``value``
       contains a non-finite float

.. function:: dumps(value, pretty_indent=0, json_mode=False)

   Serialize ``value`` into an NRDL document and return it as a string.

   Equivalent to writing ``dump(value, fh, ...)`` into a fresh
   ``io.StringIO`` and returning its contents; see :func:`dump`.

   :param value: the value to serialize
   :param pretty_indent: spaces per indentation level; ``0`` (the
       default) produces a compact document
   :type pretty_indent: int
   :param json_mode: if true, emit a valid JSON document (string keys,
       colons, and commas)
   :type json_mode: bool
   :raises TypeError: if ``value`` contains an unsupported type
   :raises ValueError: if ``pretty_indent`` is negative or ``value``
       contains a non-finite float

.. class:: NrdlError

   Raised when a document cannot be parsed as NRDL. A subclass of
   :class:`ValueError`; the message includes the line and column of the
   error.
