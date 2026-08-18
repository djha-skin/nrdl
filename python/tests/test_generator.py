"""Unit tests for nrdl.dumps: scalars, keys-as-symbols, pretty
printing, and json-mode output."""

import json

import pytest

import nrdl


# --- scalars --------------------------------------------------------

def test_none():
    assert nrdl.dumps(None) == "null"


def test_true():
    assert nrdl.dumps(True) == "true"


def test_false():
    assert nrdl.dumps(False) == "false"


def test_int():
    assert nrdl.dumps(15) == "15"


def test_negative_int():
    assert nrdl.dumps(-10) == "-10"


def test_float():
    assert nrdl.dumps(15.0) == "15.0"


def test_float_round_trip():
    assert nrdl.loads(nrdl.dumps(1.01)) == 1.01


def test_string():
    assert nrdl.dumps("hello") == '"hello"'


def test_string_with_escapes():
    assert nrdl.dumps('a\nb\t"c"') == '"a\\nb\\t\\"c\\""'


def test_string_with_unicode():
    assert nrdl.dumps("café") == '"café"'


def test_unsupported_type_raises():
    with pytest.raises(TypeError):
        nrdl.dumps(object())


def test_nan_raises():
    with pytest.raises(ValueError):
        nrdl.dumps(float("nan"))


def test_negative_pretty_indent_raises():
    with pytest.raises(ValueError):
        nrdl.dumps({}, pretty_indent=-1)


# --- keys are written as symbols ------------------------------------

def test_string_key_written_as_bareword_symbol():
    assert nrdl.dumps({"a": 1}) == "{a 1}"


def test_key_with_space_written_backtick_quoted():
    assert nrdl.dumps({"a b": 1}) == "{`a b` 1}"


def test_key_that_is_a_literal_written_as_string():
    # `true` would reparse as the boolean True, so it must be quoted.
    assert nrdl.dumps({"true": 1}) == '{"true" 1}'


def test_empty_key_written_as_string():
    assert nrdl.dumps({"": 1}) == '{"" 1}'


def test_number_key():
    assert nrdl.dumps({1: "one"}) == '{1 "one"}'


def test_boolean_key():
    assert nrdl.dumps({True: "yes"}) == '{true "yes"}'


def test_none_key():
    assert nrdl.dumps({None: "maybe"}) == '{null "maybe"}'


def test_key_with_backtick_written_backtick_quoted():
    assert nrdl.dumps({"a`b": 1}) == "{`a\\`b` 1}"


def test_string_values_are_quoted_not_symbols():
    assert nrdl.dumps({"the-wind": "bullseye"}) == '{the-wind "bullseye"}'


def test_keys_are_sorted_for_determinism():
    assert nrdl.dumps({"b": 1, "a": 2}) == "{a 2 b 1}"


# --- pretty printing ------------------------------------------------

def test_pretty_object():
    assert nrdl.dumps({"a": 1}, pretty_indent=4) == "{\n    a 1\n}"


def test_pretty_nested_object():
    assert (
        nrdl.dumps({"a": {"b": 1}}, pretty_indent=4)
        == "{\n    a {\n        b 1\n    }\n}"
    )


def test_pretty_array():
    assert nrdl.dumps([1, 2, 3], pretty_indent=4) == "[\n    1\n    2\n    3\n]"


def test_pretty_indent_zero_is_compact():
    assert nrdl.dumps({"a": [1, 2]}) == "{a [1 2]}"


# --- json mode ------------------------------------------------------

def test_json_mode_object():
    assert nrdl.dumps({"a": 1, "b": [1, 2]}, json_mode=True) == '{"a":1,"b":[1,2]}'


def test_json_mode_pretty():
    assert (
        nrdl.dumps({"a": 1, "b": [1, 2]}, json_mode=True, pretty_indent=4)
        == '{\n    "a": 1,\n    "b": [\n        1,\n        2\n    ]\n}'
    )


def test_json_mode_output_is_valid_json():
    value = {
        "a": 1,
        "b": [1.5, -2],
        "c": ["x", "y z"],
        "d": None,
        "e": False,
        "f": True,
    }
    assert json.loads(nrdl.dumps(value, json_mode=True)) == value


def test_json_mode_keys_are_quoted_strings():
    assert nrdl.dumps({"a b": 1}, json_mode=True) == '{"a b":1}'


def test_json_mode_round_trip_with_json():
    value = {"name": "Daniel", "nested": {"list": [1, 2, 3], "flag": True}}
    assert json.loads(nrdl.dumps(value, json_mode=True, pretty_indent=2)) == value
