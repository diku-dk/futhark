#!/usr/bin/env bash

FILE="tests_adhoc/ffi/test.fut"
BACKEND=c
FUTHARK="cabal run futhark --"

# Compile the server executable
$FUTHARK ${BACKEND} --server $FILE

# Expression
tests=(
# Preloaded primitives
  "p1"   "1"
  "p2"   "2"
  "p3"   "3"

# Preloaded records
  "r1"  "{x = 1, y = 2}"
  "r2"  "{x = 3, y = 4}"
  "r3"  "{x = 5, y = 6}"

# Preloaded sums
  "s1"  "#a 2"
  "s2"  "#b 4"
  "s3"  "#c 6"

# Preloaded primitive arrays
  "pa1[0]"   "1"
  "pa1[1]"   "2"
  "pa1[2]"   "3"
  "pa2[1,0]" "3"
  "pa2[1,1]" "2"
  "pa2[1,2]" "1"

# Preloaded record arrays
  "ra1[0].x"   "1"
  "ra1[1].x"   "3"
  "ra1[2].x"   "5"
  "ra2[1,0].y" "6"
  "ra2[1,1].y" "4"
  "ra2[1,2].y" "2"

# Preloaded sum arrays
  "sa1[0]"   "#a 2"
  "sa1[1]"   "#b 4"
  "sa1[2]"   "#c 6"
  "sa2[1,0]" "#c 6"
  "sa2[1,1]" "#b 4"
  "sa2[1,2]" "#a 2"

# Primitive functions
  "pf 2"   "4"
  "pf 3"   "9"

# Record functions
  "rf {x = 1, y = 2}"   "{x = 1, y = 4}"
  "rf {x = 2, y = 1}"   "{x = 4, y = 3}"

# Sum functions
  "sf (#a 2)"   "#c 3"
  "sf (#b 2)"   "#b 4"

# Primitive array functions
  "(pa1f [1,2,3])[0]"   "1"
  "(pa1f [1,2,3])[1]"   "4"
  "(pa1f [1,2,3])[2]"   "9"
  "(pa2f [[1,2,3], [3,2,1]])[0,0]"   "1"
  "(pa2f [[1,2,3], [3,2,1]])[0,1]"   "4"
  "(pa2f [[1,2,3], [3,2,1]])[0,2]"   "3"

# TODO: Write record and sum array function tests
)

for ((i=0; i<${#tests[@]}; i+=2)); do
  exp="${tests[i]}"
  expected="${tests[i+1]}"

  output=$($FUTHARK eval --backend=${BACKEND} --skip-compilation -f "$FILE" "$exp" | tr '\n' ' ' | xargs)

  if [[ "$output" == "$expected" ]]; then
    echo "PASS: $exp"
  else
    echo "FAIL: $exp (expected '$expected', got '$output')"
  fi
done
