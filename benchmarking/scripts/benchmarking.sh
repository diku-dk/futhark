      
#!/bin/bash

# Exit on error, treat unset variables as an error, and ensure pipe failures are caught.
# 'set -e' means the script will exit immediately if a command exits with a non-zero status.
# 'set -u' means it will exit if it tries to use an uninitialized variable.
# 'set -o pipefail' means a pipeline will exit with the status of the last command to exit with a non-zero status,
# or zero if all commands in the pipeline exit successfully.
#set -e
#set -u
#set -o pipefail

LOGGING_ENABLED=false

# --- Configuration ---
# These variables can be overridden by setting them as environment variables before running the script.
# Example: NUM_DRY_RUNS=5 ./your_script_name.sh

# Number of dry (warmup) runs for hyperfine
NUM_DRY_RUNS=${NUM_DRY_RUNS:-5}
# Number of actual (measured) runs for hyperfine
NUM_ACTUAL_RUNS=${NUM_ACTUAL_RUNS:-20}
# Location of the 'new' Futhark binary
NEW_FUTHARK_LOC=${NEW_FUTHARK_LOC:-"/usr/local/bin/futhark_new"}
# Location of the 'old' Futhark binary
OLD_FUTHARK_LOC=${OLD_FUTHARK_LOC:-"/usr/local/bin/futhark_og"}
# Location of the hyperfine binary
HYPERFINE_LOC=${HYPERFINE_LOC:-"hyperfine"} # Assumed to be in PATH by default
# User-specified part of the output directory path.
# Example: USER_SPECIFIED_OUTPUT_DIR_PREFIX="./my_project_benchmarks"
# This will result in "./my_project_benchmarks/benchmark_out_YYYYMMDD_HHMMSS"
# Default: "." results in "./benchmark_out_YYYYMMDD_HHMMSS"
USER_SPECIFIED_OUTPUT_DIR_PREFIX=${USER_SPECIFIED_OUTPUT_DIR_PREFIX:-"../results/hyperfine_benching_out"}
# Directory containing the .fut files to be tested
TEST_FILES_DIR=${TEST_FILES_DIR:-"/Users/jacobsiegumfeldt/Desktop/Bachelorprojekt/futhark_bsc/futhark-benchmarks/rodinia/myocyte/"}

# --- Dynamic Configuration & Script Globals ---
# These are determined at runtime or accumulate results during the script's execution.

# Output directory with a unique timestamp to avoid overwriting previous results
OUTPUT_DIR="${USER_SPECIFIED_OUTPUT_DIR_PREFIX}/benchmark_out_$(date +%Y%m%d_%H%M%S)"
# Overall log file for the entire script run, stored within the unique OUTPUT_DIR
OUTPUT_LOG="${OUTPUT_DIR}/benchmark_result.log"

# Stores the value of the 'longest' run of a given hyperfine run.
# This is the maximum of (max(mean_new_futhark_time, mean_old_futhark_time)) across all files.
MAX_TIME=0.0
# The .fut file that produced the MAX_TIME
MAX_TIME_FILE=""
# Counter for wins by the new Futhark binary
NEW_FUTHARK_WINS=0
# Counter for wins by the old Futhark binary
OLD_FUTHARK_WINS=0
# Count of .fut files processed (or attempted)
PROCESSED_FILES_COUNT=0
# Count of benchmarks that failed (e.g., hyperfine error or issues parsing its results)
FAILED_BENCHMARK_COUNT=0
# Total .fut files found to benchmark
TOTAL_FILES_FOUND=0

# Parse command line arguments
while getopts "l" flag; do
    case "${flag}" in
        l) LOGGING_ENABLED=true ;;
        *) ;;
    esac
done

# --- Helper Functions ---

# log_message: Prints a message to both the console (stdout) and the main log file.
# Console messages are prepended with a timestamp for better traceability during long runs.
# Usage: log_message "Your informative message here"
log_message() {
    local message="$1"
    if [ "${LOGGING_ENABLED}" = true ]; then
        # Print to console with a timestamp
        echo "$(date +'%Y-%m-%d %H:%M:%S') - ${message}"
        # Append raw message to the log file
        echo "${message}" >> "${OUTPUT_LOG}"
    fi
}

# log_error: Prints an error message to both the console (stderr) and the main log file.
# Console messages are prepended with "ERROR:" and a timestamp.
# Usage: log_error "A description of the error that occurred"
log_error() {
    local error_message="$1"
    # Print to console (stderr) with a timestamp and ERROR prefix
    echo "$(date +'%Y-%m-%d %H:%M:%S') - ERROR: ${error_message}" >&2
    if [ "${LOGGING_ENABLED}" = true ]; then
        # Only log to file if logging is enabled
        echo "ERROR: ${error_message}" >> "${OUTPUT_LOG}"
    fi
}

# --- Setup and Sanity Checks ---
# This function prepares the environment, checks for necessary tools and paths,
# and initializes the logging system.
setup() {
    if [ "${LOGGING_ENABLED}" = true ]; then
        # Create the main output directory and initialize logging only when enabled
        mkdir -p "${OUTPUT_DIR}"
        # Initialize the main log file
        echo "Benchmark Run Log - $(date)" > "${OUTPUT_LOG}"
        echo "==================================================" >> "${OUTPUT_LOG}"
        
        log_message "Starting benchmark script..."
        log_message "--- Configuration ---"
        log_message "NUM_DRY_RUNS: ${NUM_DRY_RUNS}"
        log_message "NUM_ACTUAL_RUNS: ${NUM_ACTUAL_RUNS}"
        log_message "NEW_FUTHARK_LOC: $(realpath "${NEW_FUTHARK_LOC}" 2>/dev/null || echo "${NEW_FUTHARK_LOC}")"
        log_message "OLD_FUTHARK_LOC: $(realpath "${OLD_FUTHARK_LOC}" 2>/dev/null || echo "${OLD_FUTHARK_LOC}")"
        log_message "HYPERFINE_LOC: ${HYPERFINE_LOC}"
        log_message "TEST_FILES_DIR: $(realpath "${TEST_FILES_DIR}" 2>/dev/null || echo "${TEST_FILES_DIR}")"
        log_message "USER_SPECIFIED_OUTPUT_DIR_PREFIX: $(realpath "${USER_SPECIFIED_OUTPUT_DIR_PREFIX}" 2>/dev/null || echo "${USER_SPECIFIED_OUTPUT_DIR_PREFIX}")"
        log_message "OUTPUT_DIR: $(realpath "${OUTPUT_DIR}" 2>/dev/null || echo "${OUTPUT_DIR}")"
        log_message "OUTPUT_LOG: $(realpath "${OUTPUT_LOG}" 2>/dev/null || echo "${OUTPUT_LOG}")"
        log_message "----------------------"
    fi

    # Check for hyperfine executable
    if ! command -v "${HYPERFINE_LOC}" &> /dev/null; then
        log_error "'${HYPERFINE_LOC}' could not be found. Please install hyperfine or ensure HYPERFINE_LOC is correct."
        exit 1
    fi
    log_message "Found hyperfine: $(command -v "${HYPERFINE_LOC}")"

    # Check for jq (JSON processor)
    if ! command -v jq &> /dev/null; then
        log_error "'jq' could not be found. Please install jq, as it is required for parsing hyperfine's JSON output."
        exit 1
    fi
    log_message "Found jq: $(command -v jq)"

    # Check Futhark binaries' existence and executability
    if [ ! -x "${NEW_FUTHARK_LOC}" ]; then
        log_error "New Futhark binary not found or not executable at '${NEW_FUTHARK_LOC}'."
        exit 1
    fi
    if [ ! -x "${OLD_FUTHARK_LOC}" ]; then
        log_error "Old Futhark binary not found or not executable at '${OLD_FUTHARK_LOC}'."
        exit 1
    fi
     # Check test files directory existence
    if [ ! -d "${TEST_FILES_DIR}" ]; then
        log_error "Test files directory not found at '${TEST_FILES_DIR}'."
        exit 1
    fi

    log_message "Setup complete. All checks passed and environment is ready."
    echo "==================================================" >> "${OUTPUT_LOG}" # Separator in log file
}

# --- Main Processing Logic ---
# This function iterates through the .fut files, runs benchmarks,
# parses results, and updates statistics.
main_processing() {
    # 2. Identify all the .fut files in TEST_FILES_DIR
    local fut_files_list=() # Initialize an empty array to store file paths
    # Use find to locate .fut files and read them into the array.
    # -print0 and read -d $'\0' handle filenames with spaces or special characters.
    while IFS= read -r -d $'\0' file; do
        fut_files_list+=("$file")
    done < <(find "${TEST_FILES_DIR}" -name "*.fut" -type f -print0)

    TOTAL_FILES_FOUND=${#fut_files_list[@]} # Store the total count of found files

    if [ "${TOTAL_FILES_FOUND}" -eq 0 ]; then
        log_message "No .fut files found in '${TEST_FILES_DIR}'. Nothing to benchmark."
        return # Exit main_processing, script will proceed to summarize.
    fi
    log_message "Found ${TOTAL_FILES_FOUND} .fut files to benchmark in '${TEST_FILES_DIR}'."

    # 3. Benchmark loop: Iterate over each found .fut file
    for test_file_full_path in "${fut_files_list[@]}"; do
        ((PROCESSED_FILES_COUNT++)) # Increment counter for attempted files
        local test_file_basename
        test_file_basename=$(basename "${test_file_full_path}")
        # Construct a prefix for output files related to this specific .fut file
        local output_file_prefix="${OUTPUT_DIR}/${test_file_basename%.fut}" # Removes .fut extension

        # 3.5. Log the file being benchmarked
        log_message "" # Adds a little spacing in the log for readability
        log_message "Benchmarking [${PROCESSED_FILES_COUNT}/${TOTAL_FILES_FOUND}]: ${test_file_full_path}"

        # Define the commands to be benchmarked
        # Environment variables are set for Futhark to control debugging/logging verbosity.
        local cmd_new="'${NEW_FUTHARK_LOC}' check '${test_file_full_path}'"
        local cmd_old="'${OLD_FUTHARK_LOC}' check '${test_file_full_path}'"
        
        # Define paths for hyperfine's output files for this test case
        local hyperfine_json_output="${output_file_prefix}.hyperfine.json"
        local hyperfine_md_output="${output_file_prefix}.hyperfine.md" # For human-readable summary

        # Create output directory if it doesn't exist (needed for hyperfine output)
        mkdir -p "${OUTPUT_DIR}"
        
        # Run hyperfine
        # The --command-name arguments are important as they are used by jq to parse the JSON output.
        # We capture hyperfine's stderr to a temporary file to log it in case of failure.
        local hyperfine_stderr_tmp
        hyperfine_stderr_tmp=$(mktemp)

        if ! "${HYPERFINE_LOC}" --warmup "${NUM_DRY_RUNS}" --runs "${NUM_ACTUAL_RUNS}" \
            --command-name "NewFUTHARK" "${cmd_new}" \
            --command-name "OldFUTHARK" "${cmd_old}" \
            --export-json "${hyperfine_json_output}" \
            --export-markdown "${hyperfine_md_output}" 2> "${hyperfine_stderr_tmp}"; then # Capture stderr
            
            log_error "Hyperfine failed for '${test_file_full_path}'."
            if [ -s "${hyperfine_stderr_tmp}" ]; then # Check if stderr temp file has content
                log_error "Hyperfine stderr: $(cat "${hyperfine_stderr_tmp}")"
            fi
            rm -f "${hyperfine_stderr_tmp}" # Clean up temp file
            ((FAILED_BENCHMARK_COUNT++))
            continue # Skip to the next file
        fi
        rm -f "${hyperfine_stderr_tmp}" # Clean up temp file on success too

        # Verify that hyperfine produced the JSON output file
        if [ ! -f "${hyperfine_json_output}" ]; then
            log_error "Hyperfine JSON output not found at '${hyperfine_json_output}' for '${test_file_full_path}', though hyperfine reported success."
            ((FAILED_BENCHMARK_COUNT++))
            continue
        fi

        # Parse hyperfine JSON output using jq to extract mean execution times
        local mean_new_str mean_old_str
        mean_new_str=$(jq -r '.results[] | select(.command == "NewFUTHARK") | .mean' "${hyperfine_json_output}")
        mean_old_str=$(jq -r '.results[] | select(.command == "OldFUTHARK") | .mean' "${hyperfine_json_output}")

        # Validate parsed mean times
        if [ -z "${mean_new_str}" ] || [ "${mean_new_str}" == "null" ]; then
            log_error "Could not parse mean time for New Futhark for '${test_file_full_path}' from '${hyperfine_json_output}'."
            ((FAILED_BENCHMARK_COUNT++))
            continue
        fi
        if [ -z "${mean_old_str}" ] || [ "${mean_old_str}" == "null" ]; then
            log_error "Could not parse mean time for Old Futhark for '${test_file_full_path}' from '${hyperfine_json_output}'."
            ((FAILED_BENCHMARK_COUNT++))
            continue
        fi
        
        # 6. Store the mean time of the new Futhark run in CUR_TIME (as per prompt, using mean_new_str directly) and log it.
        # CUR_TIME is implicitly mean_new_str for this section.
        log_message "  New Futhark mean time (CUR_TIME for this file): ${mean_new_str} s"
        log_message "  Old Futhark mean time: ${mean_old_str} s"

        # 7. Find the 'winner' (faster binary) for this file and update win counts
        local winner="Tie or indeterminable"
        # bc is used for floating-point comparisons. It outputs 1 if true, 0 if false.
        if (( $(echo "${mean_new_str} < ${mean_old_str}" | bc -l) )); then
            winner="New Futhark"
            ((NEW_FUTHARK_WINS++))
        elif (( $(echo "${mean_old_str} < ${mean_new_str}" | bc -l) )); then
            winner="Old Futhark"
            ((OLD_FUTHARK_WINS++))
        else # Times are equal
            winner="Tie (times are equal)"
        fi
        log_message "  Winner for ${test_file_basename}: ${winner}"

        # 8. Determine the "longest run component" for this file (max of its two means)
        #    and check if it's larger than the overall MAX_TIME.
        local current_file_longest_run_component
        if (( $(echo "${mean_new_str} > ${mean_old_str}" | bc -l) )); then
            current_file_longest_run_component="${mean_new_str}"
        else
            current_file_longest_run_component="${mean_old_str}"
        fi

        if (( $(echo "${current_file_longest_run_component} > ${MAX_TIME}" | bc -l) )); then
            MAX_TIME="${current_file_longest_run_component}"
            MAX_TIME_FILE="${test_file_full_path}"
            log_message "  New overall MAX_TIME: ${MAX_TIME} s (from file ${MAX_TIME_FILE}, based on its slower command mean)"
        fi
    done
}

# --- Summary ---
# This function prints the aggregated results and final statistics of the benchmark run.
summarize_results() {
    log_message "" # Spacing for readability
    log_message "--- Benchmark Summary ---"
    log_message "Total .fut files found: ${TOTAL_FILES_FOUND}"
    log_message "Total .fut files processed/attempted: ${PROCESSED_FILES_COUNT}"
    
    local successful_benchmarks=$((PROCESSED_FILES_COUNT - FAILED_BENCHMARK_COUNT))
    log_message "Successfully benchmarked files (hyperfine completed and results parsed): ${successful_benchmarks}"
    log_message "Failed benchmarks (hyperfine error or result parsing issue): ${FAILED_BENCHMARK_COUNT}"
    log_message ""

    log_message "New Futhark wins: ${NEW_FUTHARK_WINS}"
    log_message "Old Futhark wins: ${OLD_FUTHARK_WINS}"
    # Ties are files successfully benchmarked where neither was strictly faster, or times were equal.
    local ties=$((successful_benchmarks - NEW_FUTHARK_WINS - OLD_FUTHARK_WINS))
    # Ensure ties is not negative if something went wrong with counters (e.g. if wins are counted for failed runs)
    if [ $ties -lt 0 ]; then ties=0; fi 
    log_message "Ties (or equal performance): ${ties}"
    log_message ""

    if [ "${successful_benchmarks}" -gt 0 ]; then
        if [ -n "${MAX_TIME_FILE}" ]; then # Check if MAX_TIME_FILE was set
            log_message "Longest benchmark component time (MAX_TIME): ${MAX_TIME} s"
            log_message "File associated with MAX_TIME: ${MAX_TIME_FILE}"
        else
            # This case might occur if all runs had 0.0s time or MAX_TIME remained 0.0
            log_message "MAX_TIME: ${MAX_TIME} s (MAX_TIME_FILE not set, check individual logs if unexpected)"
        fi
    else
         log_message "MAX_TIME: N/A (No successful benchmarks to determine MAX_TIME)"
    fi

    log_message "--- End of Summary ---"
    log_message "Individual hyperfine JSON and Markdown reports are located in: ${OUTPUT_DIR}"
    log_message "This overall summary and detailed logs are in: ${OUTPUT_LOG}"
}


# --- Script Execution ---

# Call setup function to initialize and check environment
setup

# Call main processing function to run benchmarks
main_processing

# Call summary function to print final results
summarize_results

log_message "Benchmark script finished."

# Exit code: 0 if no benchmarks failed, or the number of failed benchmarks otherwise.
# This can be useful for CI/CD systems or automated scripting.
if [ "${FAILED_BENCHMARK_COUNT}" -gt 0 ]; then
    exit "${FAILED_BENCHMARK_COUNT}" # Exit with the count of failures
else
    exit 0 # Success
fi