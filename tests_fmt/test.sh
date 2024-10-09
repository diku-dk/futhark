#!/bin/sh

if [ $# != 1 ]; then
    echo "Usage: $0 <DIR>"
    exit 1
fi

THREADS=16
TEST_DIR="TEMP_FORMATTER_TEST"

mkdir "$TEST_DIR"
find "$1" -name '*.fut' -exec cp --parents \{\} "$TEST_DIR" \;

find "$TEST_DIR" -name '*.fut' | xargs -P $THREADS -I {} sh -c '
    prog="{}"
    if [ ! -d "$prog" ]; then
        name=${prog%.fut}
        futhark fmt "$prog" 2>/dev/null > "$name.fmt.fut"
        futhark fmt "$prog" 2>/dev/null > "$name.fmt.fmt.fut"
        futhark hash "$prog" 2>/dev/null > "$prog.expected"
        futhark hash "$name.fmt.fut" 2>/dev/null > "$prog.actual"

        tree_result=0
        idempotent_result=0

        if ! cmp --silent "$prog.expected" "$prog.actual"
        then
            tree_result=1
            echo "Failed Tree Comparison Test" >> "$name.log"
        fi
        printf "$tree_result" > "$name.tree.result"

        if ! cmp --silent "$name.fmt.fut" "$name.fmt.fmt.fut"
        then
            idempotent_result=1
            echo "Failed Idempotent Comparison Test" >> "$name.log"
        fi
        printf "$idempotent_result" > "$name.idempotent.result"

        if [ "$tree_result" -eq 0 ] && [ "$idempotent_result" -eq 0 ]
        then
            rm "$prog.expected" "$prog.actual" "$prog" "$name.fmt.fut" "$name.fmt.fmt.fut"
        fi
    fi
'

idempotent_pass=0
idempotent_fail=0
tree_pass=0
tree_fail=0

for file in $(find "$TEST_DIR" -name '*.result')
do
    if [ -d $file ]
    then
        :
    else
        if [[ "$file" == *.tree.result ]]
        then
            if [ "$(cat "$file")" = "0" ]
            then
                ((tree_pass++))
            else
                ((tree_fail++))
            fi
        elif [[ "$file" == *.idempotent.result ]]
        then
            if [ "$(cat "$file")" = "0" ]
            then
                ((idempotent_pass++))
            else
                ((idempotent_fail++))
            fi
        else
            :
        fi
    fi
done

echo "Tree Tests Passed: $tree_pass/$((tree_pass + tree_fail))"
echo "Idempotent Tests Passed: $idempotent_pass/$((idempotent_pass + idempotent_fail))"
