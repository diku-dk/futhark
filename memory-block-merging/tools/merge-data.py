#!/usr/bin/env python3
#
# Merge all the data gathered through running './gather-data.sh' or
# './gather-data-coarse.sh'.
#
# The sole argument should be the directory containing the gathered data.  This
# script will output a new file 'full.json' in the same directory.
#
# When this script has completed, you can run './summarize.py' to get a
# pretty-print version of the JSON file, and './plot.py' to plot the data.


import sys
import os
import json
import itertools
import functools
import numpy as np


# SETTINGS

# Ignore datasets with runtimes below 1 millisecond.
runtime_limit_ignore = 1000.0


# INPUT
data_dir = sys.argv[1]

def load(path):
    with open(os.path.join(data_dir, 'runs', path)) as f:
        return json.load(f)

enabling_attributes = ['without', 'with']
attributes = ['coalescing', 'reuse']

enabling_attributes_products = list(itertools.product(
    *([enabling_attributes] * len(attributes))))

variants = ['_'.join('{}-{}'.format(e, a)
                     for e, a in zip(p, attributes))
            for p in enabling_attributes_products]

print('Variants:', variants)

compilation_raw = {}
for v in variants:
    path = 'compilation_' + v + '.json'
    try:
        compilation_raw[v] = load(path)
    except IOError:
        pass

measurements_raw = {}
for v in variants:
    path = 'measurements_' + v + '.json'
    try:
        measurements_raw[v] = load(path)
    except IOError:
        pass


# DATA BUILDING

benchmark_names_sets = []
for comp in compilation_raw.values():
    benchmark_names_sets.append(set(comp.keys()))
for meas in measurements_raw.values():
    benchmark_names_sets.append(set(meas.keys()))
# These benchmarks exist in all datasets.
benchmark_names = list(functools.reduce(set.intersection, benchmark_names_sets))

dataset_names = {}
for benchmark_name in benchmark_names:
    dataset_names_sets = []
    for meas in measurements_raw.values():
        dataset_names_local = []
        for dataset_name, dataset_info in meas[benchmark_name]['datasets'].items():
            if isinstance(dataset_info, dict):
                dataset_names_local.append(dataset_name)
        dataset_names_sets.append(set(dataset_names_local))
    # These datasets exist in all benchmark runs.
    dataset_names[benchmark_name] = list(functools.reduce(set.intersection,
                                                          dataset_names_sets))

def get_dataset_info_base(benchmarks, benchmark_name, dataset_name):
    raw = benchmarks[benchmark_name]['datasets'][dataset_name]

    average_runtime = np.mean(raw['runtimes'])
    if average_runtime > runtime_limit_ignore:
        dataset_info_base = {
            'average_runtime': average_runtime,
            'total_cumulative_allocations': raw['total_allocated'],
            'total_cumulative_frees': raw['total_freed'],
            'peak_memory_usages': raw['peak_memory_usages']
        }
        return dataset_info_base
    else:
        return None

def cut_desc(s):
    if s[0] == '#':
        return s.split(' ')[0]
    else:
        return os.path.split(s)[1] # Probably safe.

benchmarks = {}
for benchmark_name in benchmark_names:
    compilation_info = {}
    for v in variants:
        try:
            compilation_info[v] = compilation_raw[v][benchmark_name]
        except KeyError:
            pass

    datasets = {}
    for dataset_name in dataset_names[benchmark_name]:
        dataset_name_short = cut_desc(dataset_name)
        dataset_info = {}
        for v in variants:
            try:
                measurements = measurements_raw[v]
                dataset_info[v] = get_dataset_info_base(measurements,
                                                        benchmark_name, dataset_name)
            except KeyError:
                pass
        if not None in dataset_info.values():
            datasets[dataset_name_short] = dataset_info

    if len(datasets) > 0:
        benchmark_info = {'compilation': compilation_info,
                          'datasets': datasets}
        benchmarks[benchmark_name] = benchmark_info

with open(os.path.join(data_dir, 'full.json'), 'w') as f:
    json.dump(benchmarks, f, sort_keys=True,
              indent=2, ensure_ascii=False)
