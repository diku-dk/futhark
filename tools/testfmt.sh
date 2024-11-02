#!/bin/sh

THREADS=16
TEST_DIR="TEMP_FORMATTER_TEST"

if [ "$TESTFMT_WORKER" ];
then
    shift
    testwith() {
        prog="$1"

        if [ -d "$prog" ]; then
            exit 0
        fi

        if ! futhark check-syntax "$prog" 2> /dev/null > /dev/null; then
            rm "$prog"
            exit 0
        fi

        name=${prog%.fut}
        futhark hash "$prog" 2> /dev/null > "$prog.expected"

        if [ ! $? -eq 0 ]; then
            rm "$prog" "$prog.expected"
            exit 0
        fi

        futhark fmt < "$prog" 2> /dev/null > "$name.fmt.fut"
        futhark fmt < "$name.fmt.fut" 2> /dev/null > "$name.fmt.fmt.fut"
        futhark hash "$name.fmt.fut" 2> /dev/null > "$prog.actual"
        futhark tokens "$prog" 2> /dev/null | grep '^COMMENT' > "$name.comments"
        futhark tokens "$name.fmt.fut" 2> /dev/null | grep '^COMMENT' > "$name.fmt.comments"

        hash_result=1
        idempotent_result=1
        comments_result=1

        if ! cmp --silent "$prog.expected" "$prog.actual"
        then
            hash_result=0
            echo "Failed Hash Comparison Test" >> "$name.log"
        fi
        printf "$hash_result" >> "$name.result"

        if ! cmp --silent "$name.fmt.fut" "$name.fmt.fmt.fut"
        then
            idempotent_result=0
            echo "Failed Idempotent Comparison Test" >> "$name.log"
        fi
        printf "$idempotent_result" >> "$name.result"

        if ! cmp --silent "$name.comments" "$name.fmt.comments"
        then
            comments_result=0
            echo "Failed Order of Comments Test" >> "$name.log"
        fi
        printf "$comments_result" >> "$name.result"
    }
    for f in "$@"; do
        testwith "$f"
    done
else
    if [ $# != 1 ]; then
        echo "Usage: $0 <DIR>"
        exit 1
    fi

    rm -rf "$TEST_DIR" && mkdir "$TEST_DIR"
    find "$1" -name '*.fut' -exec cp --parents \{\} "$TEST_DIR" \;

    export TESTFMT_WORKER=1
    find "$TEST_DIR" -name '*.fut' -print0 | xargs -0 -n 1 -P $THREADS "$0" -rec

    idempotent_pass=0
    idempotent_fail=0
    hash_pass=0
    hash_fail=0
    comments_pass=0
    comments_fail=0

    for file in $(find "$TEST_DIR" -name '*.result'); do
        if ! [ -d $file ]; then
            content=$(cat "$file")
            hash_result=$(printf "$content" | cut -c1)
            idempotent_result=$(printf "$content" | cut -c2)
            comments_result=$(printf "$content" | cut -c3)

            hash_pass=$((hash_pass + hash_result))
            hash_fail=$((hash_fail + 1 - hash_result))
            idempotent_pass=$((idempotent_pass + idempotent_result))
            idempotent_fail=$((idempotent_fail + 1 - idempotent_result))
            comments_pass=$((comments_pass + comments_result))
            comments_faul=$((comments_pass + 1 - comments_result))

            if [ "$hash_result" -eq 1 ] && [ "$idempotent_result" -eq 1 ] && [ "$comments_result" -eq 1 ]; then
                rm "${file%.*}."*
            fi
        fi
    done

    find "$TEST_DIR" -type d -empty -delete

    echo "Hash Tests Passed: $hash_pass/$((hash_pass + hash_fail))"
    echo "Idempotent Tests Passed: $idempotent_pass/$((idempotent_pass + idempotent_fail))"
    echo "Order of Comments Tests Passed: $comments_pass/$((comments_pass + comments_fail))"

    if [ $hash_fail -eq 0 ] && [ $idempotent_fail -eq 0 ] && [ $comments_fail -eq 0 ]; then
        exit 0;
    else
        exit 1;
    fi
fi
