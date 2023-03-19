#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Futhark documentation build configuration file, created by
# sphinx-quickstart on Tue Mar 24 14:21:12 2015.
#
# This file is execfile()d with the current directory set to its
# containing dir.
#
# Note that not all possible configuration values are present in this
# autogenerated file.
#
# All configuration values have a default; values that are commented out
# serve to show the default.

import sys
import os
import re
from pygments.lexer import RegexLexer, bygroups
from pygments import token
from pygments import unistring as uni
from pygments.token import (
    Text,
    Comment,
    Operator,
    Keyword,
    Name,
    String,
    Number,
    Punctuation,
    Whitespace,
)
from sphinx.highlighting import lexers

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
# sys.path.insert(0, os.path.abspath('.'))

# -- General configuration ------------------------------------------------

# If your documentation needs a minimal Sphinx version, state it here.
# needs_sphinx = '1.0'

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = ["sphinx.ext.todo", "sphinx.ext.mathjax"]

# Add any paths that contain templates here, relative to this directory.
templates_path = ["_templates"]

# The suffix of source filenames.
source_suffix = ".rst"

# The encoding of source files.
# source_encoding = 'utf-8-sig'

# The master toctree document.
master_doc = "index"

# General information about the project.
project = "Futhark"
copyright = "2013-2020, DIKU, University of Copenhagen"

# The version info for the project you're documenting, acts as replacement for
# |version| and |release|, also used in various other places throughout the
# built documents.
#
# The short X.Y version.


# No reason for a cabal file parser; let's just hack it.
def get_version():
    # Get cabal file
    cabal_file = open("../futhark.cabal", "r").read()
    # Extract version
    return re.search(
        r"^version:[ ]*([^ ]*)$", cabal_file, flags=re.MULTILINE
    ).group(1)


version = get_version()
# The full version, including alpha/beta/rc tags.
release = version

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
# language = None

# There are two options for replacing |today|: either, you set today to some
# non-false value, then it is used:
# today = ''
# Else, today_fmt is used as the format for a strftime call.
# today_fmt = '%B %d, %Y'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
exclude_patterns = ["_build", "lib"]

# The reST default role (used for this markup: `text`) to use for all
# documents.
# default_role = None

# If true, '()' will be appended to :func: etc. cross-reference text.
# add_function_parentheses = True

# If true, the current module name will be prepended to all description
# unit titles (such as .. function::).
# add_module_names = True

# If true, sectionauthor and moduleauthor directives will be shown in the
# output. They are ignored by default.
# show_authors = false

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = "sphinx"


class FutharkLexer(RegexLexer):
    """
    A Futhark lexer

    .. versionadded:: 2.8
    """

    name = "Futhark"
    url = "https://futhark-lang.org/"
    aliases = ["futhark"]
    filenames = ["*.fut"]
    mimetypes = ["text/x-futhark"]

    num_types = (
        "i8",
        "i16",
        "i32",
        "i64",
        "u8",
        "u16",
        "u32",
        "u64",
        "f32",
        "f64",
    )

    other_types = ("bool",)

    reserved = (
        "if",
        "then",
        "else",
        "def",
        "let",
        "loop",
        "in",
        "with",
        "type",
        "val",
        "entry",
        "for",
        "while",
        "do",
        "case",
        "match",
        "include",
        "import",
        "module",
        "open",
        "local",
        "assert",
        "_",
    )

    ascii = (
        "NUL",
        "SOH",
        "[SE]TX",
        "EOT",
        "ENQ",
        "ACK",
        "BEL",
        "BS",
        "HT",
        "LF",
        "VT",
        "FF",
        "CR",
        "S[OI]",
        "DLE",
        "DC[1-4]",
        "NAK",
        "SYN",
        "ETB",
        "CAN",
        "EM",
        "SUB",
        "ESC",
        "[FGRU]S",
        "SP",
        "DEL",
    )

    num_postfix = r"(%s)?" % "|".join(num_types)

    identifier_re = "[a-zA-Z_][a-zA-Z_0-9']*"

    # opstart_re = '+\-\*/%=\!><\|&\^'

    tokens = {
        "root": [
            (r"--(.*?)$", Comment.Single),
            (r"\s+", Whitespace),
            (r"\(\)", Punctuation),
            (r"\b(%s)(?!\')\b" % "|".join(reserved), Keyword.Reserved),
            (
                r"\b(%s)(?!\')\b" % "|".join(num_types + other_types),
                Keyword.Type,
            ),
            # Identifiers
            (r"#\[([a-zA-Z_\(\) ]*)\]", Comment.Preproc),
            (r"[#!]?(%s\.)*%s" % (identifier_re, identifier_re), Name),
            (r"\\", Operator),
            (r"[-+/%=!><|&*^][-+/%=!><|&*^.]*", Operator),
            (r"[][(),:;`{}?.\']", Punctuation),
            #  Numbers
            (
                r"0[xX]_*[\da-fA-F](_*[\da-fA-F])*_*[pP][+-]?\d(_*\d)*"
                + num_postfix,
                Number.Float,
            ),
            (
                r"0[xX]_*[\da-fA-F](_*[\da-fA-F])*\.[\da-fA-F](_*[\da-fA-F])*"
                r"(_*[pP][+-]?\d(_*\d)*)?" + num_postfix,
                Number.Float,
            ),
            (r"\d(_*\d)*_*[eE][+-]?\d(_*\d)*" + num_postfix, Number.Float),
            (
                r"\d(_*\d)*\.\d(_*\d)*(_*[eE][+-]?\d(_*\d)*)?" + num_postfix,
                Number.Float,
            ),
            (r"0[bB]_*[01](_*[01])*" + num_postfix, Number.Bin),
            (r"0[xX]_*[\da-fA-F](_*[\da-fA-F])*" + num_postfix, Number.Hex),
            (r"\d(_*\d)*" + num_postfix, Number.Integer),
            #  Character/String Literals
            (r"'", String.Char, "character"),
            (r'"', String, "string"),
            #  Special
            (r"\[[a-zA-Z_\d]*\]", Keyword.Type),
            (r"\(\)", Name.Builtin),
        ],
        "character": [
            # Allows multi-chars, incorrectly.
            (r"[^\\']'", String.Char, "#pop"),
            (r"\\", String.Escape, "escape"),
            ("'", String.Char, "#pop"),
        ],
        "string": [
            (r'[^\\"]+', String),
            (r"\\", String.Escape, "escape"),
            ('"', String, "#pop"),
        ],
        "escape": [
            (r'[abfnrtv"\'&\\]', String.Escape, "#pop"),
            (r"\^[][" + uni.Lu + r"@^_]", String.Escape, "#pop"),
            ("|".join(ascii), String.Escape, "#pop"),
            (r"o[0-7]+", String.Escape, "#pop"),
            (r"x[\da-fA-F]+", String.Escape, "#pop"),
            (r"\d+", String.Escape, "#pop"),
            (r"(\s+)(\\)", bygroups(Whitespace, String.Escape), "#pop"),
        ],
    }


lexers["futhark"] = FutharkLexer()

highlight_language = "text"

# A list of ignored prefixes for module index sorting.
# modindex_common_prefix = []

# If true, keep warnings as "system message" paragraphs in the built documents.
# keep_warnings = false


# -- Options for HTML output ----------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
html_theme = "futhark"
html_theme_path = ["_theme"]

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
html_theme_options = {}

# Add any paths that contain custom themes here, relative to this directory.
# html_theme_path = []

# The name for this set of Sphinx documents.  If None, it defaults to
# "<project> v<release> documentation".
# html_title = None

# A shorter title for the navigation bar.  Default is the same as html_title.
# html_short_title = None

# The name of an image file (relative to this directory) to place at the top
# of the sidebar.
# html_logo = None

# The name of an image file (within the static path) to use as favicon of the
# docs.  This file should be a Windows icon file (.ico) being 16x16 or 32x32
# pixels large.
# html_favicon = None

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
# html_static_path = ['_static']

# Add any extra paths that contain custom files (such as robots.txt or
# .htaccess) here, relative to this directory. These files are copied
# directly to the root of the documentation.
# html_extra_path = []

# If not '', a 'Last updated on:' timestamp is inserted at every page bottom,
# using the given strftime format.
# html_last_updated_fmt = '%b %d, %Y'

# If true, SmartyPants will be used to convert quotes and dashes to
# typographically correct entities.
# html_use_smartypants = True

# Custom sidebar templates, maps document names to template names.
html_sidebars = {
    "**": [
        "globaltoc.html",
        "relations.html",
        "sourcelink.html",
        "searchbox.html",
    ]
}

# Additional templates that should be rendered to pages, maps page names to
# template names.
# html_additional_pages = {}

# If false, no module index is generated.
# html_domain_indices = True

# If false, no index is generated.
# html_use_index = True

# If true, the index is split into individual pages for each letter.
# html_split_index = false

# If true, links to the reST sources are added to the pages.
# html_show_sourcelink = True

# If true, "Created using Sphinx" is shown in the HTML footer. Default is True.
# html_show_sphinx = True

# If true, "(C) Copyright ..." is shown in the HTML footer. Default is True.
# html_show_copyright = True

# If true, an OpenSearch description file will be output, and all pages will
# contain a <link> tag referring to it.  The value of this option must be the
# base URL from which the finished HTML is served.
# html_use_opensearch = ''

# This is the file name suffix for HTML files (e.g. ".xhtml").
# html_file_suffix = None

# Output file base name for HTML help builder.
htmlhelp_basename = "Futharkdoc"


# -- Options for LaTeX output ---------------------------------------------

latex_elements = {
    # The paper size ('letterpaper' or 'a4paper').
    #'papersize': 'letterpaper',
    # The font size ('10pt', '11pt' or '12pt').
    #'pointsize': '10pt',
    # Additional stuff for the LaTeX preamble.
    #'preamble': '',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
    ("index", "Futhark.tex", "Futhark User's Guide", "DIKU", "manual"),
]

# The name of an image file (relative to this directory) to place at the top of
# the title page.
# latex_logo = None

# For "manual" documents, if this is true, then toplevel headings are parts,
# not chapters.
# latex_use_parts = false

# If true, show page references after internal links.
# latex_show_pagerefs = false

# If true, show URL addresses after external links.
# latex_show_urls = false

# Documents to append as an appendix to all manuals.
# latex_appendices = []

# If false, no module index is generated.
# latex_domain_indices = True


# -- Options for manual page output ---------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ("man/futhark", "futhark", "a parallel functional array language", [], 1),
    (
        "man/futhark-autotune",
        "futhark-autotune",
        "calibrate run-time parameters",
        [],
        1,
    ),
    ("man/futhark-c", "futhark-c", "compile Futhark to sequential C", [], 1),
    (
        "man/futhark-multicore",
        "futhark-multicore",
        "compile Futhark to multithreaded C",
        [],
        1,
    ),
    (
        "man/futhark-ispc",
        "futhark-ispc",
        "compile Futhark to multithreaded ISPC",
        [],
        1,
    ),
    (
        "man/futhark-opencl",
        "futhark-opencl",
        "compile Futhark to OpenCL",
        [],
        1,
    ),
    ("man/futhark-cuda", "futhark-cuda", "compile Futhark to CUDA", [], 1),
    (
        "man/futhark-python",
        "futhark-python",
        "compile Futhark to sequential Python",
        [],
        1,
    ),
    (
        "man/futhark-pyopencl",
        "futhark-pyopencl",
        "compile Futhark to Python and OpenCL",
        [],
        1,
    ),
    (
        "man/futhark-wasm",
        "futhark-wasm",
        "compile Futhark to WebAssembly",
        [],
        1,
    ),
    (
        "man/futhark-wasm-multicore",
        "futhark-wasm-multicore",
        "compile Futhark to parallel WebAssembly",
        [],
        1,
    ),
    ("man/futhark-run", "futhark-run", "interpret Futhark program", [], 1),
    (
        "man/futhark-repl",
        "futhark-repl",
        "interactive Futhark read-eval-print-loop",
        [],
        1,
    ),
    ("man/futhark-test", "futhark-test", "test Futhark programs", [], 1),
    (
        "man/futhark-bench",
        "futhark-bench",
        "benchmark Futhark programs",
        [],
        1,
    ),
    (
        "man/futhark-doc",
        "futhark-doc",
        "generate documentation for Futhark code",
        [],
        1,
    ),
    (
        "man/futhark-dataset",
        "futhark-dataset",
        "generate random data sets",
        [],
        1,
    ),
    ("man/futhark-pkg", "futhark-pkg", "manage Futhark packages", [], 1),
    (
        "man/futhark-literate",
        "futhark-literate",
        "execute literate Futhark program",
        [],
        1,
    ),
]

# If true, show URL addresses after external links.
# man_show_urls = false


# -- Options for Texinfo output -------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
    (
        "index",
        "Futhark",
        "Futhark Documentation",
        "DIKU",
        "Futhark",
        "One line description of project.",
        "Miscellaneous",
    ),
]

# Documents to append as an appendix to all manuals.
# texinfo_appendices = []

# If false, no module index is generated.
# texinfo_domain_indices = True

# How to display URL addresses: 'footnote', 'no', or 'inline'.
# texinfo_show_urls = 'footnote'

# If true, do not generate a @detailmenu in the "Top" node's menu.
# texinfo_no_detailmenu = false
