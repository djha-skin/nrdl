# Configuration file for the Sphinx documentation builder.
#
# These docs are built by ReadTheDocs from the repository root
# (see .readthedocs.yaml at the repository root).

project = "nrdl"
copyright = "2026, Daniel Jay Haskin"
author = "Daniel Jay Haskin"
release = "0.2.0"

extensions = []

templates_path = ["_templates"]
exclude_patterns = ["_build", "Thumbs.db", ".DS_Store"]

html_theme = "alabaster"

# The nrdl logo lives with the other documentation assets at the repository
# root (docs/assets/nrdl.png); these paths are relative to this conf.py.
html_logo = "../../docs/assets/nrdl.png"
html_favicon = "../../docs/assets/nrdl.png"
