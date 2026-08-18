API reference
=============

.. function:: parse_from(text)

   Deserialize one NRDL value from the string ``text`` and return it as
   Python data.

   Objects become dictionaries, arrays become lists, numbers become
   ``int``/``float``, quoted strings become strings, and symbols become
   plain strings. The symbols ``true``, ``false``, and ``null`` become
   ``True``, ``False``, and ``None``.

   :param text: an NRDL document containing a single value
   :type text: str
   :raises NrdlError: if ``text`` is not a single valid NRDL value

.. function:: generate_to(value, pretty_indent=0, json_mode=False)

   Serialize ``value`` into an NRDL document and return it as a string.

   ``value`` may be any combination of dictionaries, lists, tuples,
   strings, numbers, booleans, and ``None``. Dictionaries become NRDL
   objects with their **keys written as symbols**; lists and tuples
   become arrays; strings become quoted strings; ``True``, ``False``,
   and ``None`` become ``true``, ``false``, and ``null``.

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
