#!/bin/sh

cabal build > /dev/null

# Check if cabal build failed
if [ $? -ne 0 ]; then
    echo "cabal build failed. Exiting."
    exit 1
fi

echo "Project Build Running Tests"

# Execute tests
cabal exec -- sh tests_fmt/test.sh tests