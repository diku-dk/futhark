#!/bin/sh
test_dir="TEMP"
diff_error=0

rm -rf "$test_dir" && mkdir "$test_dir"
for file in *.fut; do
    fmtFile=$test_dir/$(basename -s .fut $file).fmt.fut
    futhark fmt < $file > $fmtFile
    if ! cmp --silent expected/$file $fmtFile; then
        echo "$file didn't format as expected"
        diff_error=1
    else
        rm $fmtFile
    fi
done

exit $diff_error
