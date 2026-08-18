"""Unit tests for nrdl.generate_to: scalars, keys-as-symbols, pretty
printing, and json-mode output."""

import json

import pytest

import nrdl


# --- scalars --------------------------------------------------------

def test_none():
    assert nrdl.generate_to(None) == "null"


def test_true():
    assert nrdl.generate_to(True) == "true"


def test_false():
    assert nrdl.generate_to(False) == "false"


def test_int():
    assert nrdl.generate_to(15) == "15"


def test_negative_int():
    assert nrdl.generate_to(-10) == "-10"


def test_float():
    assert nrdl.generate_to(15.0) == "15.0"


def test_float_round_trip():
    assert nrdl.parse_from(nrdl.generate_to(1.01)) == 1.01


def test_string():
    assert nrdl.generate_to("hello") == '"hello"'


def test_string_with_escapes():
    assert nrdl.generate_to('a\nb\t"c"') == '"a\\nb\\t\\"c\\""'


def test_string_with_unicode():
    assert nrdl.generate_to("café") == '"café"'


def test_unsupported_type_raises():
    with pytest.raises(TypeError):
        nrdl.generate_to(object())


def test_nan_raises():
    with pytest.raises(ValueError):
        nrdl.generate_to(float("nan"))


def test_negative_pretty_indent_raises():
    with pytest.raises(ValueError):
        nrdl.generate_to({}, pretty_indent=-1)


# --- keys are written as symbols ------------------------------------

def test_string_key_written_as_bareword_symbol():
    assert nrdl.generate_to({"a": 1}) == "{a 1}"


def test_key_with_space_written_backtick_quoted():
    assert nrdl.generate_to({"a b": 1}) == "{`a b` 1}"


def test_key_that_is_a_literal_written_as_string():
    # `true` would reparse as the boolean True, so it must be quoted.
    assert nrdl.generate_to({"true": 1}) == '{"true" 1}'


def test_empty_key_written_as_string():
    assert nrdl.generate_to({"": 1}) == '{"" 1}'


def test_number_key():
    assert nrdl.generate_to({1: "one"}) == '{1 "one"}'


def test_boolean_key():
    assert nrdl.generate_to({True: "yes"}) == '{true "yes"}'


def test_none_key():
    assert nrdl.generate_to({None: "maybe"}) == '{null "maybe"}'


def test_key_with_backtick_written_backtick_quoted():
    assert nrdl.generate_to({"a`b": 1}) == "{`a\\`b` 1}"


def test_string_values_are_quoted_not_symbols():
    assert nrdl.generate_to({"the-wind": "bullseye"}) == '{the-wind "bullseye"}'


def test_keys_are_sorted_for_determinism():
    assert nrdl.generate_to({"b": 1, "a": 2}) == "{a 2 b 1}"


# --- pretty printing ------------------------------------------------

def test_pretty_object():
    assert nrdl.generate_to({"a": 1}, pretty_indent=4) == "{\n    a 1\n}"


def test_pretty_nested_object():
    assert (
        nrdl.generate_to({"a": {"b": 1}}, pretty_indent=4)
        == "{\n    a {\n        b 1\n    }\n}"
    )


def test_pretty_array():
    assert nrdl.generate_to([1, 2, 3], pretty_indent=4) == "[\n    1\n    2\n    3\n]"


def test_pretty_indent_zero_is_compact():
    assert nrdl.generate_to({"a": [1, 2]}) == "{a [1 2]}"


# --- json mode ------------------------------------------------------

def test_json_mode_object():
    assert nrdl.generate_to({"a": 1, "b": [1, 2]}, json_mode=True) == '{"a":1,"b":[1,2]}'


def test_json_mode_pretty():
    assert (
        nrdl.generate_to({"a": 1, "b": [1, 2]}, json_mode=True, pretty_indent=4)
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
    assert json.loads(nrdl.generate_to(value, json_mode=True)) == value


def test_json_mode_keys_are_quoted_strings():
    assert nrdl.generate_to({"a b": 1}, json_mode=True) == '{"a b":1}'


def test_json_mode_round_trip_with_json():
    value = {"name": "Daniel", "nested": {"list": [1, 2, 3], "flag": True}}
    assert json.loads(nrdl.generate_to(value, json_mode=True, pretty_indent=2)) == value
