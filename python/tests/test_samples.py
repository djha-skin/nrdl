"""Parse every .nrdl file in samples/ and compare to its expected value.

Each sample file is a single NRDL value derived from the ABNF in the
repository README, paired with a ``<name>.expected.py`` file containing
a Python literal of the value it should deserialize to.
"""

import ast
from pathlib import Path

import pytest

import nrdl

SAMPLES = Path(__file__).parent / "samples"


def sample_pairs():
    pairs = []
    for nrdl_path in sorted(SAMPLES.glob("*.nrdl")):
        expected_path = nrdl_path.with_suffix(".expected.py")
        if not expected_path.exists():
            pytest.fail("missing expected file for %s" % nrdl_path.name)
        pairs.append(pytest.param(nrdl_path, expected_path, id=nrdl_path.stem))
    return pairs


@pytest.mark.parametrize("nrdl_path, expected_path", sample_pairs())
def test_sample_parses(nrdl_path, expected_path):
    text = nrdl_path.read_text(encoding="utf-8")
    expected = ast.literal_eval(expected_path.read_text(encoding="utf-8"))
    assert nrdl.loads(text) == expected


@pytest.mark.parametrize("nrdl_path, expected_path", sample_pairs())
def test_sample_round_trips(nrdl_path, expected_path):
    """Parsing, serializing, and parsing again yields the same value."""
    text = nrdl_path.read_text(encoding="utf-8")
    value = nrdl.loads(text)
    assert nrdl.loads(nrdl.dumps(value)) == value
