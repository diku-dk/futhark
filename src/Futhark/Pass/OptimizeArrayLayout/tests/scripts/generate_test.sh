# Do not run this script unless you know what you are doing.
# For each FILE.fut in "/Users/bak/Desktop/Speciale/futhark/tests_analysis",
# generate a test file by computing the IR of the program and get the
# expected output by running the pass on the generated IR.
# NOTE: this will overwrite the existing test files, and make the tests
# trivially pass. To generate the tests, the pass should be disabled in
# the pipeline.

overwrite=false

dir0="/Users/bak/Desktop/Speciale/futhark/tests_analysis"
dir1="/Users/bak/Desktop/Speciale/futhark/src/Futhark/Pass/OptimizeArrayLayout/tests"

# Get path of the latest build of the futhark executable
futhark_dev=$(ls -tr $(find . -name futhark -type f ) | tail -1)

# loop counter to break after first iteration
i=0

for file in "$dir0"/*.fut_gpu
do

    i=$((i+1))
    # if [ $i -eq 1 ]
    # then
    #     continue
    # fi
    # if [ $i -eq 3 ]
    # then
    #     break
    # fi

    # Check if the file already has a test file
    if [ -f "$dir1/$(basename -- "$file")" ] && [ "$overwrite" = false ]; then
        printf "Test file already exists for $file. Skipping.\n"
        continue
    fi
    printf "Generating test for $file\n"

    # Get result
    res=$($futhark_dev dev $file)
    # Pipe result into file
    new_file="$dir1/$(basename -- "$file")"
    echo "$res" > "$new_file"

    # Append the following to the file
    string="\n\n-- === Expected output after pass:\n"
    printf "$string" >> "$dir1/$(basename -- "$file")"

    # Get expected output
    pass_res=$($futhark_dev dev --coalesce --simplify --cse $new_file)
    # Prepend each line of the result with "-- "
    pass_res=$(echo "$pass_res" | sed -Ee 's/^/-- /;s/ +$//')
    # Append expected output to file
    echo "$pass_res" >> "$new_file"

done
