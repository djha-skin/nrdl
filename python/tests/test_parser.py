"""Unit tests for nrdl.loads: numbers, literals, strings, symbols,
multi-line blobs, comments, and error handling."""

import pytest

import nrdl


# --- numbers --------------------------------------------------------

def test_integer():
    assert nrdl.loads("15") == 15


def test_negative_integer():
    assert nrdl.loads("-10") == -10


def test_zero():
    assert nrdl.loads("0") == 0


def test_float():
    assert nrdl.loads("1.01") == 1.01


def test_exponent():
    assert nrdl.loads("1e5") == 100000.0


def test_negative_exponent():
    assert nrdl.loads("2.5e-3") == 0.0025


def test_number_after_minus_requires_int():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads("-")


def test_leading_zero_not_allowed():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads("01")


def test_double_decimal_point():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads("1.2.3")


def test_dot_is_not_a_number():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads(".5")


def test_number_plus_trailing_bareword_is_trailing_content():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads("15x")


# --- literals -------------------------------------------------------

def test_true_is_True():
    assert nrdl.loads("true") is True


def test_false_is_False():
    assert nrdl.loads("false") is False


def test_null_is_None():
    assert nrdl.loads("null") is None


def test_backtick_true_is_also_True():
    # Per the README, `true` and true are equivalent.
    assert nrdl.loads("`true`") is True


def test_backtick_null_is_also_None():
    assert nrdl.loads("`null`") is None


# --- symbols --------------------------------------------------------

def test_bareword_is_a_string():
    assert nrdl.loads("the-wind") == "the-wind"


def test_symbols_are_plain_strings():
    for symbol in ("__dunder__", "+constant+", "/path/to/thing", "<tag>", "*star*"):
        assert nrdl.loads(symbol) == symbol


def test_plus_prefixed_string_is_a_symbol_not_a_number():
    assert nrdl.loads("+5") == "+5"


def test_backtick_symbol_is_a_string():
    assert nrdl.loads("`force push`") == "force push"


def test_backtick_symbol_with_escaped_backtick():
    assert nrdl.loads("`a\\`b`") == "a`b"


def test_backtick_symbol_with_unicode_escape():
    assert nrdl.loads("`\\u00e9`") == "é"


def test_empty_backtick_symbol_is_an_error():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads("``")


# --- quoted strings -------------------------------------------------

def test_quoted_string():
    assert nrdl.loads('"hello"') == "hello"


def test_string_escapes():
    assert nrdl.loads(r'"a\nb\t\"c\" \\ \/ \u0041"') == 'a\nb\t"c" \\ / A'


def test_string_unicode_escape():
    assert nrdl.loads(r'"\u0041\u0042"') == "AB"


def test_string_bad_escape_is_an_error():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads(r'"\x"')


def test_string_bad_unicode_escape_is_an_error():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads(r'"\u12zz"')


def test_unterminated_string_is_an_error():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads('"hello')


# --- multi-line strings ---------------------------------------------

def test_verbatim_string():
    assert nrdl.loads("|a\n|b\n^") == "a\nb"


def test_verbatim_trailing_empty_line():
    assert nrdl.loads("|a\n|b\n|\n^") == "a\nb\n"


def test_verbatim_with_blank_lines_between():
    assert nrdl.loads("|a\n\n|b\n^") == "a\nb"


def test_verbatim_with_comments_between():
    assert nrdl.loads("|a\n# comment\n|b\n^") == "a\nb"


def test_verbatim_content_keeps_hash():
    # Comments cannot appear on the same line as a multi-line string.
    assert nrdl.loads("|a # not a comment\n^\n") == "a # not a comment"


def test_prose_folds_lines_to_spaces():
    assert nrdl.loads(">a\n>b\n>c\n^") == "a b c"


def test_prose_trailing_space_stays():
    assert nrdl.loads(">a \n>b\n^") == "a  b"


def test_unterminated_verbatim_is_an_error():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads("|a\n|b")


def test_multiline_requires_matching_marker():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads("|a\n>b\n^")


# --- arrays ---------------------------------------------------------

def test_empty_array():
    assert nrdl.loads("[]") == []


def test_array_of_symbols():
    assert nrdl.loads("[a b c]") == ["a", "b", "c"]


def test_array_with_commas():
    assert nrdl.loads("[1, 2, 3]") == [1, 2, 3]


def test_array_with_colons():
    assert nrdl.loads("[1: 2: 3]") == [1, 2, 3]


def test_nested_arrays():
    assert nrdl.loads("[[1 2] [3 4]]") == [[1, 2], [3, 4]]


def test_heterogeneous_array():
    assert nrdl.loads('[1 "two" three true null]') == [1, "two", "three", True, None]


def test_array_missing_separator_is_an_error():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads('[a"b"]')


def test_unterminated_array_is_an_error():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads("[1 2")


# --- objects --------------------------------------------------------

def test_empty_object():
    assert nrdl.loads("{}") == {}


def test_object_with_bareword_keys():
    assert nrdl.loads("{a 1 b 2}") == {"a": 1, "b": 2}


def test_object_json_style():
    assert nrdl.loads('{"a": 1, "b": 2}') == {"a": 1, "b": 2}


def test_object_with_number_keys():
    assert nrdl.loads('{1 "one" 2 "two"}') == {1: "one", 2: "two"}


def test_object_with_literal_keys():
    assert nrdl.loads('{true "yes" null "no"}') == {True: "yes", None: "no"}


def test_object_missing_key_value_separator_is_an_error():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads('{a"b"}')


def test_object_with_odd_number_of_values_is_an_error():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads("{a 1 b}")


def test_unterminated_object_is_an_error():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads("{a 1")


# --- comments and whitespace ----------------------------------------

def test_leading_comment():
    assert nrdl.loads("# hi\n42") == 42


def test_trailing_comment():
    assert nrdl.loads("42 # done\n") == 42


def test_comment_between_values_in_object():
    assert nrdl.loads("{a # comment\n 1}") == {"a": 1}


def test_surrounding_whitespace_is_ignored():
    assert nrdl.loads("  \n\t [a]  ") == ["a"]


def test_empty_input_is_an_error():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads("")


def test_comment_only_input_is_an_error():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads("# just a comment\n")


def test_two_top_level_values_is_an_error():
    with pytest.raises(nrdl.NrdlError):
        nrdl.loads("{a 1}{b 2}")
