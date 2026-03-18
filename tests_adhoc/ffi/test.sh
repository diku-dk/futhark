#!/usr/bin/env bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FILE="$DIR/test.fut"

# Compile the server executable
futhark c --server $FILE

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
  "pa1[0] + 0"   "1"
  "pa1[1] + 0"   "2"
  "pa1[2] + 0"   "3"
  "pa2[1,0] + 0" "3"
  "pa2[1,1] + 0" "2"
  "pa2[1,2] + 0" "1"

# Preloaded record arrays
  "ra1[0].x + 0"   "1"
  "ra1[1].x + 0"   "3"
  "ra1[2].x + 0"   "5"
  "ra2[1,0].y + 0" "6"
  "ra2[1,1].y + 0" "4"
  "ra2[1,2].y + 0" "2"

# Preloaded sum arrays
  "sa1[0]"   "ex" # TODO: I will add this when I auto-realize output values
  "sa1[1]"   "ex" # TODO: I will add this when I auto-realize output values
  "sa1[2]"   "ex" # TODO: I will add this when I auto-realize output values
  "sa2[1,0]" "ex" # TODO: I will add this when I auto-realize output values
  "sa2[1,1]" "ex" # TODO: I will add this when I auto-realize output values
  "sa2[1,2]" "ex" # TODO: I will add this when I auto-realize output values

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
  "(pa1f [1,2,3])[0] + 0"   "1"
  "(pa1f [1,2,3])[1] + 0"   "4"
  "(pa1f [1,2,3])[2] + 0"   "9"
  "(pa2f [[1,2,3], [3,2,1]])[0,0] + 0"   "1"
  "(pa2f [[1,2,3], [3,2,1]])[0,1] + 0"   "4"
  "(pa2f [[1,2,3], [3,2,1]])[0,2] + 0"   "3"

# TODO: Record and sum array functions
)

for ((i=0; i<${#tests[@]}; i+=2)); do
  exp="${tests[i]}"
  expected="${tests[i+1]}"

  output=$(futhark eval -f "$DIR/test.fut" "$exp" | tr '\n' ' ' | xargs)

  if [[ "$output" == "$expected" ]]; then
    echo "PASS: $exp"
  else
    echo "FAIL: $exp (expected '$expected', got '$output')"
  fi
done
