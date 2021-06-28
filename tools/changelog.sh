#!/bin/sh
#
# Extract CHANGELOG.md entries for a given version number.
#
# Usage:
#
# $ sh changelog.sh x.y.z < CHANGELOG.md

set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 x.y.z"
fi

v=$1

awk '/^## /  { thisone = index($0,V) != 0; next } \
     thisone { print }' \
    "V=$1"
