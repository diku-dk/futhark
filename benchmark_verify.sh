#!/bin/bash

# Define the path to the Artifact.hs file
ARTIFACT_FILE="unittests/Futhark/Analysis/Properties/Artifact.hs"

echo "Benchmarking Futhark 'verify' command for IndexFn properties..."
echo "------------------------------------------------------------------"

# Extract program paths
PROGRAM_PATHS=$(grep -o '".*\.fut"' "$ARTIFACT_FILE" | awk -F'"' '{print $2}')

# Check if PROGRAM_PATHS is empty
if [ -z "$PROGRAM_PATHS" ]; then
    echo "No program paths found in $ARTIFACT_FILE. Exiting."
    exit 1
fi

# Iterate through each program path and run the verify command with timing
for PROGRAM_PATH in $PROGRAM_PATHS; do
    echo "Running verify for: $PROGRAM_PATH"
    # Use time -p for portable time output (real, user, sys)
    # Directly call the futhark executable
    TIME_OUTPUT=$( { time -p futhark verify "$PROGRAM_PATH" > /dev/null; } 2>&1 )
    REAL_TIME=$(echo "$TIME_OUTPUT" | grep real | awk '{print $2}')

    if [ -z "$REAL_TIME" ]; then
        echo "  Failed to get timing for $PROGRAM_PATH. Output: $TIME_OUTPUT"
    else
        echo "  Time taken: ${REAL_TIME}s"
    fi
    echo "------------------------------------------------------------------"
done

echo "Benchmarking complete."
