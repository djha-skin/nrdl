"""Round-trip tests: serializing a parsed document and parsing it again
must yield the same value, in both NRDL and json mode."""

import pytest

import nrdl

CASES = [
    None,
    True,
    False,
    0,
    15,
    -10,
    1.01,
    "hello",
    "multi\nline",
    "tab\there",
    'quote"inside',
    "unicode ☕",
    [],
    [1, 2, 3],
    ["a", "b", "c"],
    [[1, 2], [3, 4]],
    [1, "two", True, None],
    {},
    {"a": 1},
    {"a b": 1},
    {"true": 1},
    {"": 1},
    {1: "one", 2: "two"},
    {True: "yes", None: "no"},
    {"outer": {"inner": [1, 2, 3], "s": "str"}, "other": True},
    {"nested": [{"deep": {"deeper": [1, [2, [3]]]}}]},
    {"the-wind": "bullseye", "poem": "His eye\nis on\nThe sparrow"},
    {"keys": ["with", "spaces", "and`backticks"]},
]


@pytest.mark.parametrize("value", CASES, ids=repr)
def test_round_trip(value):
    assert nrdl.loads(nrdl.dumps(value)) == value


@pytest.mark.parametrize("value", CASES, ids=repr)
def test_round_trip_pretty(value):
    assert nrdl.loads(nrdl.dumps(value, pretty_indent=4)) == value


def _json_round_trippable(value):
    """True if every dictionary key in ``value`` is a string.

    json mode writes keys as quoted strings, so non-string keys cannot
    survive a json-mode round trip."""
    if isinstance(value, dict):
        return all(isinstance(k, str) for k in value) and all(
            _json_round_trippable(v) for v in value.values()
        )
    if isinstance(value, (list, tuple)):
        return all(_json_round_trippable(v) for v in value)
    return True


JSON_CASES = [c for c in CASES if _json_round_trippable(c)]


@pytest.mark.parametrize("value", JSON_CASES, ids=repr)
def test_round_trip_json_mode(value):
    assert nrdl.loads(nrdl.dumps(value, json_mode=True)) == value


def test_example_document_round_trips():
    text = (__import__("pathlib").Path(__file__).parent / "samples" / "example.nrdl").read_text()
    value = nrdl.loads(text)
    assert nrdl.loads(nrdl.dumps(value)) == value
    assert nrdl.loads(nrdl.dumps(value, pretty_indent=4)) == value
    assert nrdl.loads(nrdl.dumps(value, json_mode=True)) == value
