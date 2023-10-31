#!/usr/bin/env bash
#
# Perform basic style checks on a single Futhark compiler source code
# file.  If a style violation is found, this script will exit with a
# nonzero exit code.  Checks performed:
#
#   0) if Haskell: hlint (with some rules ignored).
#
#   1) Trailing whitespace.
#
#   2) A file ending in blank lines.
#
#   3) Tabs, anywhere.
#
#   4) DOS line endings (CRLF).
#
#   5) If Python: black and mypy.
#
# This script can be called on directories (in which case it applies
# to every file inside), or on files.

cyan=$(printf '%b' "\033[0;36m")
NC=$(printf '%b' "\033[0m")

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <file>"
    exit 1
fi

exit=0

hlint_check() {
    # Some hlint-suggestions are terrible, so ignore them here.
    hlint -XNoCPP -i "Use import/export shortcut" -i "Use const" -i "Use tuple-section" -i "Too strict maybe" -i "Functor law" "$1"
}

no_trailing_blank_lines() {
    awk '/^$/ { sawblank=1; next } { sawblank=0 } END { if (sawblank==1) { exit 1 } }' "$1"
}

file="$1"

if grep -E -n " +$" "$file"; then
    echo
    echo "${cyan}Trailing whitespace in $file:${NC}"
    echo "$output"
    exit=1
fi

no_tabs() {
    if grep -E -n "$(printf '\t')" "$1"; then
        echo
        echo "${cyan}Tab characters found in $1:${NC}"
        echo "$output"
        exit=1
    fi
}

if file "$file" | grep -q 'CRLF line terminators'; then
    echo
    echo "${cyan}CRLF line terminators in $file.${NC}"
    exit=1
fi

if ! no_trailing_blank_lines "$file"; then
    echo
    echo "${cyan}$file ends in several blank lines.${NC}"
    exit=1
fi

case "$file" in
    *.fut)
        no_tabs "$file"
        ;;
    *.hs)
        no_tabs "$file"
        if ! LC_ALL=C.UTF-8 ormolu --mode check "$file"; then
            echo
            echo "${cyan}$file:${NC} is not formatted correctly with Ormolu"
            echo "$output"
            exit=1
        fi
        output=$(hlint_check "$file")
        if [ $? = 1 ]; then
            echo
            echo "${cyan}$file:${NC} hlint issues"
            echo "$output"
            exit=1
        fi
        ;;
    *.py)
        no_tabs "$file"
        output=$(mypy --no-error-summary "$file")
        if [ $? != 0 ]; then
            echo
            echo "${cyan}$file:${NC} Mypy is upset"
            echo "$output"
            exit=1
        fi
        output=$(black --check --diff --quiet "$file")
        if [ $? != 0 ]; then
            echo
            echo "${cyan}$file:${NC} is not formatted correctly with Black"
            echo "$output"
            exit=1
        fi
        ;;
esac

exit $exit
