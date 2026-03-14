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
    VERIFY_TIME_OUTPUT=$( { time -p futhark verify "$PROGRAM_PATH" > /dev/null; } 2>&1 )
    VERIFY_TIME=$(echo "$VERIFY_TIME_OUTPUT" | grep real | awk '{print $2}')

    if [ -z "$VERIFY_TIME" ]; then
        echo "  Failed to get verification timing for $PROGRAM_PATH. Output: $VERIFY_TIME_OUTPUT"
    else
        echo "  Verification time taken: ${VERIFY_TIME}s"

        UNANNOTATED_PROGRAM_PATH="${PROGRAM_PATH%.fut}_unannotated.fut"
        echo "Running futhark c for: $UNANNOTATED_PROGRAM_PATH"
        # Capture stdout/stderr and exit status
        COMPILE_COMMAND_OUTPUT=$( { time -p futhark c "$UNANNOTATED_PROGRAM_PATH" > /dev/null; } 2>&1 )
        COMPILE_EXIT_STATUS=$?
        COMPILE_TIME=$(echo "$COMPILE_COMMAND_OUTPUT" | grep real | awk '{print $2}')

        if [ "$COMPILE_EXIT_STATUS" -ne 0 ]; then
            echo "  futhark c failed for $UNANNOTATED_PROGRAM_PATH with exit status $COMPILE_EXIT_STATUS. Aborting processing for this program."
        elif [ -z "$COMPILE_TIME" ]; then
            echo "  Failed to get compile timing for $UNANNOTATED_PROGRAM_PATH. Output: $COMPILE_COMMAND_OUTPUT"
        else
            echo "  Compile time taken: ${COMPILE_TIME}s"
            # Calculate percentage: (VERIFY_TIME / (COMPILE_TIME + VERIFY_TIME)) * 100
            TOTAL_TIME=$(echo "$VERIFY_TIME + $COMPILE_TIME" | bc -l)
            if (( $(echo "$TOTAL_TIME == 0" | bc -l) )); then
                echo "  Total time is zero, cannot calculate percentage."
            else
                PERCENTAGE=$(echo "($VERIFY_TIME / $TOTAL_TIME) * 100" | bc -l)
                printf "  Verification time is %.2f%% of total (compile + verification) time.\n" "$PERCENTAGE"
            fi
        fi
    fi
    echo "------------------------------------------------------------------"
done

echo "Benchmarking complete."
