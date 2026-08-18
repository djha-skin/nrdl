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

The API follows the `json` module's conventions: `load`/`dump` work with
file handles, and `loads`/`dumps` work with strings.

```python
import nrdl

# Deserialize an NRDL document from a string.
doc = nrdl.loads("""
{
    the-wind "bullseye"
    the-trees false
    the-sparrows his-eye
}
""")
# -> {"the-wind": "bullseye", "the-trees": false, "the-sparrows": "his-eye"}

# Or from a file handle.
with open("config.nrdl", encoding="utf-8") as fh:
    doc = nrdl.load(fh)

# Serialize Python data back to NRDL.
nrdl.dumps(doc, pretty_indent=4)
# -> '{\n    the-sparrows "his-eye"\n    the-trees false\n    the-wind "bullseye"\n}'

# Write it to a file handle.
with open("config.out.nrdl", "w", encoding="utf-8") as fh:
    nrdl.dump(doc, fh, pretty_indent=4)

# Emit a JSON document instead.
nrdl.dumps(doc, json_mode=True)
# -> '{"the-sparrows": "his-eye", "the-trees": false, "the-wind": "bullseye"}'
```

Because Python has no symbol type, NRDL symbols deserialize to plain strings,
and `true`/`false`/`null` deserialize to `True`/`False`/`None`. When
serializing, dictionary keys are written as symbols while string values are
written as quoted strings.

## Command line

The package also installs an `nrdl` command that parses documents and prints
the parsed data, as NRDL by default or as JSON with `--json`:

```console
$ nrdl config.nrdl
{
    the-wind "bullseye"
}
$ nrdl --json config.nrdl
{
    "the-wind": "bullseye"
}
$ cat config.nrdl | nrdl
...
$ nrdl --validate config.nrdl && echo ok
ok
```

`nrdl --validate` parses documents without printing them, reporting only
problems to stderr and exiting non-zero when a document is invalid.

Full documentation is available on
[ReadTheDocs](https://nrdl.readthedocs.io/).
