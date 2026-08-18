"""Tests for multi-line string output from dump/dumps.

In pretty mode, strings containing newlines are written as verbatim
blocks (``|``-prefixed lines ending in ``^``) and long strings with
spaces are written as prose blocks (``>``-prefixed lines ending in
``^``). Minified and json-mode output always uses quoted strings.
"""

import pytest

import nrdl


def test_verbatim_top_level():
    assert nrdl.dumps("a\nb", pretty_indent=4) == "|a\n|b\n^"


def test_verbatim_trailing_newline():
    assert nrdl.dumps("a\nb\n", pretty_indent=4) == "|a\n|b\n|\n^"


def test_verbatim_blank_line():
    assert nrdl.dumps("a\n\nb", pretty_indent=4) == "|a\n|\n|b\n^"


def test_verbatim_in_object():
    assert (
        nrdl.dumps({"poem": "His eye\nis on"}, pretty_indent=4)
        == "{\n    poem\n        |His eye\n        |is on\n        ^\n}"
    )


def test_verbatim_in_array():
    assert nrdl.dumps(["a\nb"], pretty_indent=4) == "[\n    |a\n    |b\n    ^\n]"


def test_prose_top_level():
    s = "x" * 40 + " " + "y" * 40
    assert nrdl.dumps(s, pretty_indent=4) == ">" + "x" * 40 + "\n>" + "y" * 40 + "\n^"


def test_prose_folds_back_to_the_string():
    s = "x" * 40 + " " + "y" * 40
    assert nrdl.loads(nrdl.dumps(s, pretty_indent=4)) == s


def test_short_string_stays_quoted():
    assert nrdl.dumps("hello", pretty_indent=4) == '"hello"'


def test_long_word_without_spaces_stays_quoted():
    s = "x" * 200
    assert nrdl.dumps(s, pretty_indent=4) == '"%s"' % s


def test_string_with_carriage_return_stays_quoted():
    assert nrdl.dumps("a\r\nb", pretty_indent=4) == '"a\\r\\nb"'


def test_minified_always_quoted():
    assert nrdl.dumps("a\nb") == '"a\\nb"'


def test_json_mode_always_quoted():
    assert nrdl.dumps("a\nb", json_mode=True) == '"a\\nb"'
    assert nrdl.dumps("a\nb", json_mode=True, pretty_indent=4) == '"a\\nb"'


TRICKY_STRINGS = [
    "a\nb",
    "a\nb\n",
    "\n",
    "line1\nline2\nline3\n",
    ("a b c d e f g h i j k l m n o p q r s t u v w x y z " * 5),
    "x" * 150 + " y",
    ("a  b  c  d " * 30),  # runs of spaces survive the fold
    ("  leading and trailing spaces  " * 15),
    "tab\there\nnewline",
    "café\n☕",
    "ends with space " * 20,
]


@pytest.mark.parametrize("s", TRICKY_STRINGS, ids=repr)
def test_pretty_round_trip(s):
    assert nrdl.loads(nrdl.dumps(s, pretty_indent=4)) == s


@pytest.mark.parametrize("s", TRICKY_STRINGS, ids=repr)
def test_minified_round_trip(s):
    assert nrdl.loads(nrdl.dumps(s)) == s


@pytest.mark.parametrize("s", TRICKY_STRINGS, ids=repr)
def test_json_mode_round_trip(s):
    assert nrdl.loads(nrdl.dumps(s, json_mode=True)) == s


def test_blobs_in_nested_structures_round_trip():
    value = {
        "poem": "His eye\nis on\nThe sparrow",
        "other": "And I know\nHe's watching\nOver me",
        "wendover": ["this\nthat", "x" * 120 + " y z", "short"],
        "note": ("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do " * 4),
    }
    assert nrdl.loads(nrdl.dumps(value, pretty_indent=4)) == value
