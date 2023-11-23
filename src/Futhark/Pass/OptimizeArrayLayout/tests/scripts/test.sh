#!/usr/bin/env bash
# Bash script that reads the test files in the directory, runs them and compares their output to their expected output from comments in the test files.

# Print help
if [ "$1" == "-h" ] || [ "$1" == "--help" ]; then
    printf "Usage: ./test.sh [file ...] [OPTIONAL: specific test number]\n"
    exit 0
fi


# The directory where the test files are located. This is the parent directory of this script.
TEST_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && cd .. && pwd )"


# Counter for the number of successful tests.
successes=0
# Counter for the number of skipped tests.
skipped=0
# Index of the current test
test_i=0
# Number of tests
test_n=$(find "$TEST_DIR" -name '*.fut_gpu' | wc -l)

# Get path of the latest build of the futhark executable
futhark_dev=$(ls -tr $(find . -name futhark -type f ) | tail -1)

function skip_test() {
  printf "\e[33m" # Yello
  printf "SKIPPED\n"
  printf "\e[0m" # Reset
  printf "\n%s in \"%s\". \nSkipping test.\n\n" "${1}" "${2}"
  skipped=$((${skipped} + 1))
}

# Read the test files in the directory and get the number of characters in the longest filename.
# Needed for formatting the output.
max_filename_length=0
for file in "$TEST_DIR"/*.fut_gpu
do
    # Get filename without path
    filename=$(basename -- "$file")
    # Get the length of the filename
    filename_length=${#filename}
    # If the filename is longer than the longest filename, set the longest filename to the length of the current filename.
    if [ "$filename_length" -gt "$max_filename_length" ]; then
        max_filename_length=$filename_length
    fi
done

# If first argument is a number, run that test only.
specific_test_number=$1
# Check if the first argument is a number.
run_specific_test=false
if [ "$specific_test_number" -eq "$specific_test_number" ] 2>/dev/null; then
    # Check if the number is in the range of the number of tests.
    if [ "$specific_test_number" -gt 0 ] && [ "$specific_test_number" -le "$test_n" ]; then
        run_specific_test=true
        test_n=1
    fi
fi

function diff_strings() {
  local expected="${1}"
  local output="${2}"

  # Split the expected output into two string at the first difference.
  # This is done to highlight the difference in the output.

  # Iterate over the characters in the output and expected output
  # until the first difference is found.
  local output_i=0
  local expected_output_i=0
  local output_length=${#output}
  local expected_output_length=${#expected}
  local difference_location=0
  local found_difference=false
  while [ $output_i -lt "$output_length" ] && [ $expected_output_i -lt "$expected_output_length" ]
  do
    # Get the current character in the output and expected output.
    local output_char=${output:$output_i:1}
    local expected_output_char=${expected:$expected_output_i:1}

    local output_i=$((output_i+1))
    local expected_output_i=$((expected_output_i+1))

    # If the characters are different, set the difference location
    # to the current index and break the loop.
    if [ "$output_char" != "$expected_output_char" ]; then
      found_difference=true
      break
    fi

    difference_location=$((difference_location+1))
  done

  # Split the output and expected output into three strings at the difference location
  # where the the second string is the difference.
  local output_1=${output:0:difference_location}
  local output_2=${output:difference_location:1}
  local output_3=${output:difference_location+1}
  local expected_output_1=${expected:0:difference_location}
  local expected_output_2=${expected:difference_location:1}
  local expected_output_3=${expected:difference_location+1}

  # Fix newlines being weird.
  if [ "$output_2" == $'\n' ]; then
    local output_2="\e[41m \e[0m\n" # Red highlighted space
  else
    local output_2="\e[41m$output_2\e[0m" # Red highlighted character
  fi
  if [ "$expected_output_2" == $'\n' ]; then
    local expected_output_2="\e[41m \e[0m\n" # Red highlighted space
  else
    local expected_output_2="\e[41m$expected_output_2\e[0m" # Red highlighted character
  fi

  # Print the output and expected output with the difference highlighted in red
  printf "\nOutput: \n\n"
  printf "\e[37m" # Light grey
  printf "$output_1"
  printf "$output_2"
  printf "\e[37m" # Light grey
  printf "$output_3\n"
  printf "\e[0m" # Reset

  printf "\nExpected output: \n\n"
  printf "\e[37m" # Light grey
  printf "$expected_output_1"
  printf "$expected_output_2"
  printf "\e[37m" # Light grey
  printf "$expected_output_3\n\n\n"
  printf "\e[0m" # Reset
}

function wordDiff() {
  [ -z "${1}" ] && echo "expected output is empty" && return
  [ -z "${2}" ] && echo "output is empty" && return

  wdiff --start-delete="$(printf "\e[32m")" --end-delete="$(printf "\e[0m")" \
    --start-insert="$(printf "\e[31m")" --end-insert="$(printf "\e[0m")" \
    <(printf "${1}") \
    <(printf "${2}")

  printf "\n"
}

which wdiff >/dev/null

if [ $? -eq 0 ] ; then
  diffFunc=wordDiff
else
  diffFunc=diff_strings
fi

# Read the test files in the directory.
for file in "$TEST_DIR"/*.fut_gpu
do
    # Run specific test if specified.
    if [ "$run_specific_test" = true ] ; then
        # If the current test is not the specified test, skip it.
        if [ $((test_i+1)) -ne "$specific_test_number" ]; then
            test_i=$((test_i+1))
            continue
        fi
    fi

    # Increment the test index
    test_i=$((test_i+1))

    # Get filename without path
    filename=$(basename -- "$file")
    numtests=$(echo -n "$test_n" | wc -c)
    printf "\e[1m" # Bold
    printf "Running test %*d/%d: " $numtests "$test_i" "$test_n"
    printf "\e[34m" # Blue
    printf "%s" "$filename "
    printf "\e[0m" # White


    # Print spaces to align the output.
    spaces=$((max_filename_length - ${#filename}))
    for ((i=0; i<"$spaces"; i++))
    do
        printf " "
    done
    printf "\t"

    # Run the test file and compare the output to the expected output.

    # Run the test file.
    output=$($futhark_dev dev --coalesce "$file" 2>&1)
    # Remove trailing whitespace
    output=$(echo "$output" | sed 's/[[:space:]]*$//')

    # Get the expected output starting after the string "=== Expected output of analysis:"
    # Find the line where the expected output starts + 1
    expected_output_line=$(grep -n "=== Expected output after pass:" "$file" | cut -d: -f1 | awk '{print $1+1}')
    # Remove trailing whitespace
    expected_output_line=$(echo "$expected_output_line" | sed 's/[[:space:]]*$//')

    # If expected output line is empty, skip the test.
    if [ -z "$expected_output_line" ]; then
      skip_test "Expected output not specified" "$file"
      continue
    fi


    # Get the expected output.
    expected_output=$(sed -n "${expected_output_line},$ p" "$file")
    # Remove the string "-- " from the beginning of each line.
    expected_output=$(echo "$expected_output" | sed -Ee 's/^-- ?//g')

    # If expected output line is "TBD", skip the test.
    if [ "$expected_output" == "TBD" ]; then
      skip_test "Expected output is not determined yet" "$file"
      continue
    fi

    # Compare the output to the expected output.
    if [ "$output" == "$expected_output" ]; then
        successes=$((successes+1))

        # Print "CONFIRMED" if expected_output_line contains the string "CONFIRMED"
        expected_output_line_prev=$((expected_output_line-1))
        expected_output_line_content=$(sed -n "$expected_output_line_prev,$ p" "$file")
        if [[ $expected_output_line_content == *"CONFIRMED"* ]]; then
            printf "\e[32m" # Green
            printf "PASSED\n"
            printf "\e[0m" # Reset

        else
            printf "\e[33m" # Yellow
            printf "PASSED\t"

            printf "\e[35m" # Purple
            printf "(Expected output may be wrong)\n"
            printf "\e[0m" # Reset
        fi


    else
        printf "\e[31m" # Red
        printf "FAILED\n"
        printf "\e[0m\n" # Reset

        $diffFunc "${expected_output}" "${output}"
        printf "\n"
    fi
done


# Get percentage of successful tests
which wdiff &>/dev/null
if [ $? -eq 0 ] ; then
  success_percent="($(echo "scale=2; $successes/$test_n*100" | bc)%)"
fi

# Print the number of successful tests.
printf "\e[1m" # Bold
if [ $successes -eq "$test_n" ]; then
    printf "\e[32m" # Green
    printf "\nAll %d/%d %s tests passed!" "$successes" "$test_n" "$success_percent"
    [ "${skipped}" -gt 0 ] && printf "  (%d skipped)" "${skipped}"
    printf "\n\n\e[0m" # White
else
    printf "\e[31m" # Red
    printf "\n%d/%d %s tests passed." "$successes" "$test_n" "$success_percent"
    [ "${skipped}" -gt 0 ] && printf "  (%d skipped)" "${skipped}"
    printf "\n\n\e[0m" # White
fi
