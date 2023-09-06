# Bash script that reads the test files in the directory, runs them and compares their output to their expected output from comments in the test files.

# The directory where the test files are located.
TEST_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Read the test files in the directory and get the number of characters in the longest filename.
# Needed for formatting the output.
max_filename_length=0
for file in $TEST_DIR/*.fut
do 
    # Get filename without path
    filename=$(basename -- "$file")
    # Get the length of the filename
    filename_length=${#filename}
    # If the filename is longer than the longest filename, set the longest filename to the length of the current filename.
    if [ $filename_length -gt $max_filename_length ]; then
        max_filename_length=$filename_length
    fi
done

# Read the test files in the directory.
for file in $TEST_DIR/*.fut
do 
    # Get filename without path
    filename=$(basename -- "$file")
    printf "=== Running test "
    # Print with blue color
    printf "\e[34m"
    printf "$filename "
    printf "\e[0m"
    
    # Print spaces to align the output.
    spaces=$((max_filename_length - ${#filename}))
    for ((i=0; i<$spaces; i++))
    do
        printf " "
    done
    printf "\t"
    
    # futhark_dev
    futhark_dev=$(ls -tr $(find . -name futhark -type f ) | tail -1)

    # Run the test file and compare the output to the expected output.
    
    # Run the test file.
    output=$($futhark_dev dev -se --gpu -z $file 2>&1)
    # Rempove trailing whitespace
    output=$(echo "$output" | sed 's/[[:space:]]*$//')
    
    # Get the expected output starting after the string "=== Expected output of analysis:"
    # Find the line where the expected output starts + 1
    expected_output_line=$(grep -n "=== Expected output of analysis:" $file | cut -d: -f1 | awk '{print $1+1}')
    # Rempove trailing whitespace
    expected_output_line=$(echo "$expected_output_line" | sed 's/[[:space:]]*$//')

    # If expected output line is empty, skip the test.
    if [ -z "$expected_output_line" ]; then
        # Print with red color
        printf "\e[31m"
        printf "FAILED\n"
        printf "\e[0m"
        printf "\nExpected output not specified in \"$file\". \nSkipping test.\n\n"
        continue
    fi

    # Get the expected output.
    expected_output=$(sed -n "${expected_output_line},$ p" $file)
    # Remove the string "---" from the beginning of each line.
    expected_output=$(echo "$expected_output" | sed 's/^-- //g')


    # Compare the output to the expected output.
    if [ "$output" == "$expected_output" ]; then
        # Print with green color
        printf "\e[32m"
        printf "PASSED\n"
        printf "\e[0m"
    else
        # Print with red color
        printf "\e[31m"
        printf "FAILED\n"
        printf "\e[0m"
        printf "\n--- Output: \n\n$output\n"
        printf "\n--- Expected output: \n\n$expected_output\n\n\n"
    fi
done
