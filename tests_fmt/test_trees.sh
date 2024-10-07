#!/bin/sh
set -o pipefail

if [ $# != 1 ]; then
    echo "Usage: $0 <PROGS>"
    exit 1
fi

pass=0
fail=0

for prog in $(find $1 -name '*.fut')
do
    if [ -d $prog ]
    then
        :
    else
        bef_hash=$(futhark hash "$prog" 2>/dev/null)
        aft_hash=$(futhark fmt < "$prog" | futhark hash 2>/dev/null) 
        
        if [ "$bef_hash" == "$aft_hash" ]
        then
            pass=$((pass + 1))
        else
            fail=$((fail + 1))
        fi
    fi
done

sum=$((pass + fail))
echo "Passed: $pass/$sum"