#!/bin/sh

for d in * ; do
    if [ -d "$d" ]; then
        echo -n "$d: "
        if ! (cd $d && ./test.sh); then
            echo Failed
            exit 1
        else
            echo Success
        fi
    fi
done
