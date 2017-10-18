#!/bin/sh
#
# Test the auto-indentation feature of futhark-mode.el against a corpus of
# correctly indented Futhark programs.
#
# On success, write nothing and return 0.  On failure, write the errors and
# return 1.

cd "$(dirname "$0")/futhark-mode.el-corpus"

futhark_indent() {
    ../futfmt "$file"
}

for file in *.fut; do
    if ! (futhark_indent "$file" | cmp -s "$file" -); then
        had_failure=1
        echo "ERROR IN $file; expected:"
        echo '--------------------'
        cat "$file"
        echo '--------------------'
        echo 'Got:'
        echo '--------------------'
        futhark_indent "$file"
        echo '--------------------'
        echo 'Diff:'
        echo '--------------------'
        futhark_indent "$file" | diff "$file" -
        echo '--------------------'
    fi
done

if [ "$had_failure" ]; then
    exit 1
fi
