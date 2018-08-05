#!/usr/bin/env python
#
# Remove some benchmark from a JSON file produced by futhark-bench's --json option.
#
# This is useful if we accidentally added some pointless programs that
# just obscure things.

import json
import sys

remove = sys.argv[1]
files = sys.argv[2:]

removed=False
i = 0
for fp in files:
    with open(fp, 'r') as infile:
        try:
            file_json = json.load(infile)
        except Exception as e:
            print('Could not read {}: {}'.format(fp, e))
            continue
    for b in file_json.keys():
        if remove in b:
            file_json.pop(b)
            removed = True
    with open(fp, 'w') as outfile:
        json.dump(file_json, outfile)
    i += 1
    if (i % 100 == 0):
        print('{}/{} files processed...'.format(i, len(files)))

if not removed:
    print('Warning: no JSON file contained {}'.format(remove))
