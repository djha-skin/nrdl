"""Tests for the nrdl command-line interface."""

import io
import json
import sys

import pytest

import nrdl
from nrdl import cli


@pytest.fixture
def doc_file(tmp_path):
    path = tmp_path / "config.nrdl"
    path.write_text('{the-wind "bullseye" the-trees false}', encoding="utf-8")
    return path


def test_prints_parsed_document_as_nrdl(doc_file, capsys):
    status = cli.main([str(doc_file)])
    value = nrdl.loads(doc_file.read_text(encoding="utf-8"))
    assert status == 0
    assert capsys.readouterr().out == nrdl.dumps(value, pretty_indent=4) + "\n"


def test_json_flag_prints_valid_json(doc_file, capsys):
    status = cli.main(["--json", str(doc_file)])
    out = capsys.readouterr().out
    assert status == 0
    assert json.loads(out) == {"the-wind": "bullseye", "the-trees": False}


def test_indent_zero_is_compact(doc_file, capsys):
    cli.main(["--indent", "0", str(doc_file)])
    assert capsys.readouterr().out == '{the-trees false the-wind "bullseye"}\n'


def test_reads_from_stdin_when_no_file(monkeypatch, capsys):
    monkeypatch.setattr(sys, "stdin", io.StringIO("42 # the answer\n"))
    assert cli.main([]) == 0
    assert capsys.readouterr().out == "42\n"


def test_dash_reads_from_stdin(monkeypatch, capsys):
    monkeypatch.setattr(sys, "stdin", io.StringIO("|a\n|b\n^"))
    assert cli.main(["-"]) == 0
    assert capsys.readouterr().out == "|a\n|b\n^\n"


def test_parse_error_reports_to_stderr(tmp_path, capsys):
    bad = tmp_path / "bad.nrdl"
    bad.write_text("{a 1", encoding="utf-8")
    assert cli.main([str(bad)]) == 1
    captured = capsys.readouterr()
    assert captured.out == ""
    assert "nrdl:" in captured.err


def test_missing_file_reports_to_stderr(capsys):
    assert cli.main(["/nonexistent/nrdl-doc.nrdl"]) == 1
    assert "nrdl:" in capsys.readouterr().err


def test_multiple_files_continues_after_error(tmp_path, capsys):
    good = tmp_path / "good.nrdl"
    good.write_text("1", encoding="utf-8")
    bad = tmp_path / "bad.nrdl"
    bad.write_text("[", encoding="utf-8")
    assert cli.main([str(good), str(bad), str(good)]) == 1
    out = capsys.readouterr().out
    assert out.count("1\n") == 2


def test_version_flag(capsys):
    with pytest.raises(SystemExit) as exc:
        cli.main(["--version"])
    assert exc.value.code == 0
    assert capsys.readouterr().out.startswith("nrdl ")


def test_help_lists_options(capsys):
    with pytest.raises(SystemExit) as exc:
        cli.main(["--help"])
    assert exc.value.code == 0
    out = capsys.readouterr().out
    assert "--json" in out and "--indent" in out and "FILE" in out
