"""Tests for the stream-based API: load/dump take file handles, and
loads/dumps are their StringIO-backed string counterparts."""

import io

import pytest

import nrdl


# --- load -----------------------------------------------------------

def test_load_from_stringio():
    assert nrdl.load(io.StringIO("{a 1}")) == {"a": 1}


def test_load_from_open_file(tmp_path):
    path = tmp_path / "doc.nrdl"
    path.write_text('{the-wind "bullseye"}', encoding="utf-8")
    with open(path, encoding="utf-8") as fh:
        assert nrdl.load(fh) == {"the-wind": "bullseye"}


def test_load_from_stdin_like():
    assert nrdl.load(io.StringIO("42")) == 42


def test_load_reads_the_whole_document():
    assert nrdl.load(io.StringIO("# comment\n[a b] # trailing\n")) == ["a", "b"]


def test_load_rejects_invalid_document():
    with pytest.raises(nrdl.NrdlError):
        nrdl.load(io.StringIO("{a 1"))


def test_loads_equals_load_of_stringio():
    text = '{a 1 b [1 2 3] "k" true}'
    assert nrdl.loads(text) == nrdl.load(io.StringIO(text))


# --- dump -----------------------------------------------------------

def test_dump_writes_to_handle():
    out = io.StringIO()
    assert nrdl.dump({"a": 1}, out) is None
    assert out.getvalue() == "{a 1}"


def test_dump_writes_to_open_file(tmp_path):
    path = tmp_path / "out.nrdl"
    with open(path, "w", encoding="utf-8") as fh:
        nrdl.dump({"a": [1, 2]}, fh, pretty_indent=4)
    assert path.read_text(encoding="utf-8") == "{\n    a [\n        1\n        2\n    ]\n}"


def test_dump_json_mode(tmp_path):
    out = io.StringIO()
    nrdl.dump({"a": 1}, out, json_mode=True)
    assert out.getvalue() == '{"a":1}'


def test_dump_then_load_round_trips(tmp_path):
    value = {"poem": "His eye\nis on", "list": [1, True, None], "k": {"deep": "x"}}
    path = tmp_path / "roundtrip.nrdl"
    with open(path, "w", encoding="utf-8") as fh:
        nrdl.dump(value, fh, pretty_indent=4)
    with open(path, encoding="utf-8") as fh:
        assert nrdl.load(fh) == value


def test_dump_negative_indent_raises():
    with pytest.raises(ValueError):
        nrdl.dump({}, io.StringIO(), pretty_indent=-1)


def test_dump_unsupported_type_raises():
    with pytest.raises(TypeError):
        nrdl.dump(object(), io.StringIO())


# --- dumps ----------------------------------------------------------

def test_dumps_returns_string():
    assert nrdl.dumps({"a": 1}) == "{a 1}"


def test_dumps_equals_dump_to_stringio():
    value = {"a": {"b": [1, 2]}, "s": "x\ny"}
    for kwargs in (
        {},
        {"pretty_indent": 2},
        {"json_mode": True},
        {"json_mode": True, "pretty_indent": 4},
    ):
        out = io.StringIO()
        nrdl.dump(value, out, **kwargs)
        assert nrdl.dumps(value, **kwargs) == out.getvalue()


def test_dumps_multiline_strings_in_pretty_mode():
    assert nrdl.dumps("a\nb", pretty_indent=4) == "|a\n|b\n^"
