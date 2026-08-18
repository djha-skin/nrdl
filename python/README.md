# nrdl

A pure-Python implementation of [NRDL](https://github.com/djha-skin/nrdl), the
Nestable, Readable Document Language — a superset of JSON with comments,
multi-line strings, and symbols. This package is a port of the reference
implementation in Common Lisp, following the language specification and ABNF
in the repository README.

## Installation

```bash
pip install nrdl
```

## Usage

```python
import nrdl

# Deserialize an NRDL document into Python data.
doc = nrdl.parse_from("""
{
    the-wind "bullseye"
    the-trees false
    the-sparrows his-eye
}
""")
# -> {"the-wind": "bullseye", "the-trees": False, "the-sparrows": "his-eye"}

# Serialize Python data back to NRDL.
nrdl.generate_to(doc, pretty_indent=4)
# -> '{\n    the-sparrows "his-eye"\n    the-trees false\n    the-wind "bullseye"\n}'

# Emit a JSON document instead.
nrdl.generate_to(doc, json_mode=True)
# -> '{"the-sparrows": "his-eye", "the-trees": false, "the-wind": "bullseye"}'
```

Because Python has no symbol type, NRDL symbols deserialize to plain strings,
and `true`/`false`/`null` deserialize to `True`/`False`/`None`. When
serializing, dictionary keys are written as symbols while string values are
written as quoted strings.

Full documentation is available on
[ReadTheDocs](https://nrdl.readthedocs.io/).
