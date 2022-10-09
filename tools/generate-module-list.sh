#!/bin/sh
#
# This is an ad-hoc hack to generate a list of exposed modules for the
# .cabal file.  It automatically excludes the parser stuff (which go
# in the Other-Modules field in the .cabal file) and driver programs,
# under the assumption that these don't change much.  Run it from the
# root of the directory.

find "${1:-src}" -type f \
    | grep -v '#' \
    | grep -v '~' \
    | grep -E -v 'Language.Futhark.Parser.(Parser|Lexer|Tokens)|futhark|futharki' \
    | sed -e "s/.hs$//" -e "s_${1:-src}/__" -e "s_/_._g" \
    | sort
