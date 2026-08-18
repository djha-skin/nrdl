"""Command-line interface for the nrdl package.

Reads NRDL documents and prints the parsed data -- re-serialized as
NRDL by default, or as JSON with ``--json``. Documents are read from
the named files, or from standard input when no file is given (or the
file is ``-``). With ``--validate``, documents are only checked for
validity and nothing is printed.
"""

from __future__ import annotations

import argparse
import sys
from importlib import metadata

import nrdl

_EPILOG = """\
examples:
  nrdl config.nrdl          print a parsed document as NRDL
  nrdl --json config.nrdl   print a parsed document as JSON
  nrdl --validate file.nrdl check a document without printing it
  cat config.nrdl | nrdl    read a document from standard input
"""


def _version():
    try:
        return metadata.version("nrdl")
    except metadata.PackageNotFoundError:  # running from a source checkout
        return "unknown"


def build_parser():
    parser = argparse.ArgumentParser(
        prog="nrdl",
        description="Parse NRDL documents and print the parsed data.",
        epilog=_EPILOG,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("--version", action="version", version="%(prog)s " + _version())
    parser.add_argument(
        "-j",
        "--json",
        action="store_true",
        help="print the parsed data as JSON instead of NRDL",
    )
    parser.add_argument(
        "--validate",
        action="store_true",
        help="check that each document parses and print nothing on success",
    )
    parser.add_argument(
        "--indent",
        type=int,
        default=4,
        metavar="N",
        help="indent with N spaces per level (0 = compact); default 4",
    )
    parser.add_argument(
        "files",
        nargs="*",
        metavar="FILE",
        help="NRDL document(s) to parse; '-' or no file reads standard input",
    )
    return parser


def main(argv=None):
    parser = build_parser()
    args = parser.parse_args(argv)
    if args.indent < 0:
        parser.error("--indent must be >= 0")
    status = 0
    for path in args.files or [None]:
        name = path if path not in (None, "-") else "<stdin>"
        fh = None
        try:
            fh = sys.stdin if path in (None, "-") else open(path, encoding="utf-8")
            value = nrdl.load(fh)
        except (nrdl.NrdlError, OSError) as exc:
            print("nrdl: %s: %s" % (name, exc), file=sys.stderr)
            status = 1
        else:
            if args.validate:
                continue
            kwargs = {"pretty_indent": args.indent}
            if args.json:
                kwargs["json_mode"] = True
            print(nrdl.dumps(value, **kwargs))
        finally:
            if fh is not None and fh is not sys.stdin:
                fh.close()
    return status


if __name__ == "__main__":
    sys.exit(main())
