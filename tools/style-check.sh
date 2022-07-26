#!/usr/bin/env bash
#
# Run style-check-file.sh on every file in a directory tree.

set -e
set -o pipefail

check="$(dirname "$0")"/style-check-file.sh
if [ $# -ne 0 ]; then
    # Running a style checker will not contribute to a scientific
    # publication.
    find "$@" -type f | parallel --will-cite "$check"
fi
