#!/bin/fish

set test_dir "tools/webgpu-tester/tests"
set meta_file "$test_dir/test-files.json"

mkdir -p $test_dir

echo -n "[" > $meta_file

set delim ""
for p in $argv
	set f (path basename "$p")
	futhark dev --gpu-mem --test-webgpu-kernels "$p" > "$test_dir/$f.js"

	echo "$delim" >> $meta_file
	set delim ","
	echo -n "  \"$f\"" >> $meta_file
end

echo -e "\n]" >> $meta_file
