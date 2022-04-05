#!/usr/bin/env bash
#
# Perform basic style checks on a single Futhark compiler source code
# file.  If a style violation is found, this script will exit with a
# nonzero exit code.  Checks performed:
#
#   0) hlint (with some rules ignored).
#
#   1) Trailing whitespace.
#
#   2) A file ending in blank lines.
#
#   3) Tabs, anywhere.
#
#   4) DOS line endings (CRLF).
#
# This script can be called on directories (in which case it applies
# to every file inside), or on files.

RED=$(printf '%b' "\033[1;31m")
cyan=$(printf '%b' "\033[0;36m")
NC=$(printf '%b' "\033[0m")

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <file>"
    exit 1
fi

exit=0

hlintable() {
    (echo "$1" | egrep -q ".l?hs$")
}

hlint_check() {
    # Some hlint-suggestions are terrible, so ignore them here.
    hlint -XNoCPP -i "Use import/export shortcut" -i "Use const" -i "Use tuple-section" -i "Too strict maybe" "$1"
}

no_trailing_blank_lines() {
    awk '/^$/ { sawblank=1; next } { sawblank=0 } END { if (sawblank==1) { exit 1 } }' "$1"
}

file="$1"

output=$(egrep -n " +$" "$file")
if [ $? = 0 ]; then
    echo
    echo "${cyan}Trailing whitespace in $file:${NC}"
    echo "$output"
    exit=1
fi

output=$(egrep -n "$(printf '\t')" "$file")
if [ $? = 0 ]; then
    echo
    echo "${cyan}Tab characters found in $file:${NC}"
    echo "$output"
    exit=1
fi

output=$(file "$file" | grep -q 'CRLF line terminators')
if [ $? = 0 ]; then
    echo
    echo "${cyan}CRLF line terminators in $file.${NC}"
    exit=1
fi

if hlintable "$file"; then
    output=$(hlint_check "$file")
    if [ $? = 1 ]; then
        echo
        echo "${cyan}Hlint issues in $file:${NC}"
        echo "$output"
        exit=1
    fi
fi

if ! no_trailing_blank_lines "$file"; then
    echo
    echo "${cyan}$file ends in several blank lines.${NC}"
    exit=1
fi

if hlintable "$file"; then
    output=$(LC_ALL=C.UTF-8 ormolu --mode check "$file")
    if [ $? != 0 ]; then
        echo
        echo "${cyan}$file:${NC} is not formatted correctly with Ormolu"
        echo "$output"
        exit=1
    fi
fi

exit $exit
