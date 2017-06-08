#!/usr/bin/env python3

'''
Merge all the data gathered through running
'./gather-data.sh'.

The sole argument should be the directory containing the gathered data.
This script will output a new file 'full.json' in the same directory.

When this script has completed, you can run './summarize.py' to get a
pretty-print version of the JSON file, and './plot.py' to plot the data.
'''

import sys
import os
import json
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

comp_ipl_mbm = load('compilation_in-place-lowering_memory-block-merging.json')
comp_no_ipl_mbm = load('compilation_no-in-place-lowering_memory-block-merging.json')
comps = (comp_ipl_mbm, comp_no_ipl_mbm)
meas_ipl_no_mbm = load('measurements_in-place-lowering_no-memory-block-merging.json')
meas_no_ipl_no_mbm = load('measurements_no-in-place-lowering_no-memory-block-merging.json')
meas_ipl_mbm = load('measurements_in-place-lowering_memory-block-merging.json')
meas_no_ipl_mbm = load('measurements_no-in-place-lowering_memory-block-merging.json')
meass = (meas_ipl_no_mbm, meas_no_ipl_no_mbm, meas_ipl_mbm, meas_no_ipl_mbm)


# STRUCTURE IN OUTPUT JSON:
#
# top-level: { benchmark_name: benchmark_info }
#            for every benchmark_name of the benchmarks
#
# benchmark_info: { 'compilation': compilation_info
#                 , 'datasets': datasets
#                 }
#
# compilation_info: { 'with-in-place-lowering': compilation_info_base
#                   , 'without-in-place-lowering': compilation_info_base
#                   }
#                   where compilation_info_base is just whatever JSON the
#                   compiler spouts
#
# datasets: { dataset_name: dataset_info }
#           for every dataset_name of the benchmark
#
# dataset_info: { 'without-in-place-lowering-without-memory-block-merging': dataset_info_base
#               ,    'without-in-place-lowering-with-memory-block-merging': dataset_info_base
#               ,    'with-in-place-lowering-without-memory-block-merging': dataset_info_base
#               ,       'with-in-place-lowering-with-memory-block-merging': dataset_info_base
#               }
#
# dataset_info_base: { 'average_runtime': microseconds
#                    , 'total_cumulative_allocations': bytes
#                    , 'total_cumulative_frees': bytes
#                    , 'peak_memory_usages': peak_memory_usages
#                    }
#
# peak_memory_usages: { space_name: bytes }
#                     for every space_name in the available memory spaces


# DATA BUILDING

benchmark_names_sets = []
for comp in comps:
    benchmark_names_sets.append(set(comp.keys()))
for meas in meass:
    benchmark_names_sets.append(set(meas.keys()))
# These benchmarks exist in all datasets.
benchmark_names = list(functools.reduce(set.intersection, benchmark_names_sets))

dataset_names = {}
for benchmark_name in benchmark_names:
    dataset_names_sets = []
    for meas in meass:
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
    compilation_info = {
        'with-in-place-lowering': comp_ipl_mbm[benchmark_name],
        'without-in-place-lowering': comp_no_ipl_mbm[benchmark_name]
    }

    datasets = {}
    for dataset_name in dataset_names[benchmark_name]:
        d_no_ipl_no_mbm = get_dataset_info_base(meas_no_ipl_no_mbm, benchmark_name, dataset_name)
        d_no_ipl_mbm = get_dataset_info_base(meas_no_ipl_mbm, benchmark_name, dataset_name)
        d_ipl_no_mbm = get_dataset_info_base(meas_ipl_no_mbm, benchmark_name, dataset_name)
        d_ipl_mbm = get_dataset_info_base(meas_ipl_mbm, benchmark_name, dataset_name)
        if d_no_ipl_no_mbm is None or \
           d_no_ipl_mbm is None or \
           d_ipl_no_mbm is None or \
           d_ipl_mbm is None:
            continue # Ignore.

        dataset_info = {
            'without-in-place-lowering-without-memory-block-merging':
            d_no_ipl_no_mbm,
            'without-in-place-lowering-with-memory-block-merging':
            d_no_ipl_mbm,
            'with-in-place-lowering-without-memory-block-merging':
            d_ipl_no_mbm,
            'with-in-place-lowering-with-memory-block-merging':
            d_ipl_mbm
        }
        dataset_name = cut_desc(dataset_name)
        datasets[dataset_name] = dataset_info

    if len(datasets) > 0:
        benchmark_info = {'compilation': compilation_info,
                          'datasets': datasets}
        benchmarks[benchmark_name] = benchmark_info

with open(os.path.join(data_dir, 'full.json'), 'w') as f:
    json.dump(benchmarks, f, sort_keys=True,
              indent=2, ensure_ascii=False)
