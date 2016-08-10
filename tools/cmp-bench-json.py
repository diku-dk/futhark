#!/usr/bin/env python
#
# Compare to JSON files produced by futhark-bench's --json option.

import json
import sys

def mean(numbers):
    return float(sum(numbers)) / max(len(numbers), 1)

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

_, a_file, b_file = sys.argv

a_json = json.load(open(a_file))
b_json = json.load(open(b_file))

speedups = {}

# First we iterate through json_a and add to speedups everything that
# has a matching entry in json_b, and complain if we find something
# that is not in json _b.  Then we iterate through json_b and complain
# about everything that is not in json_a (but we do not add anything
# to speedups).

for prog,a_prog in a_json.items():
    if not prog in b_json:
        print('In %s but not %s: program %s' % (a_file, b_file, prog))
    else:
        a_prog_datasets = a_prog['datasets']
        b_prog_datasets = b_json[prog]['datasets']
        speedups[prog] = {}
        for dataset,a_dataset_results in a_prog_datasets.items():
            if not dataset in b_prog_datasets:
                print('In %s but not %s: program %s dataset %s' % (a_file, b_file, prog, dataset))
            else:
                b_dataset_results = b_prog_datasets[dataset]
                speedups[prog][dataset] = mean(b_dataset_results['runtimes'])/mean(a_dataset_results['runtimes'])

for prog,b_prog in b_json.items():
    if not prog in a_json:
        print('In %s but not %s: program %s' % (b_file, a_file, prog))
    else:
        b_prog_datasets = b_prog['datasets']
        a_prog_datasets = a_json[prog]['datasets']
        for dataset,b_dataset_results in b_prog_datasets.items():
            if not dataset in a_prog_datasets:
                print('In %s but not %s: program %s dataset %s' % (b_file, a_file, prog, dataset))

# Now we can report the speedups from a to b.

for prog,prog_speedups in speedups.items():
    if len(prog_speedups) > 0:
        print('\n%s%s%s' % (bcolors.HEADER+bcolors.BOLD, prog, bcolors.ENDC))
        for dataset,dataset_speedup in prog_speedups.items():
            if dataset_speedup > 1.01:
                color = bcolors.OKGREEN
            elif dataset_speedup < 0.99:
                color = bcolors.FAIL
            else:
                color = ''
            print('  %s%s%10.2fx%s' % ((dataset+':').ljust(64), color, dataset_speedup, bcolors.ENDC))
