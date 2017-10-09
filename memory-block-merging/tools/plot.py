#!/usr/bin/env python3

'''
Plot the results you get from running './gather-data-coarse.sh' and then
'./merge-data-coarse.py'.

The sole argument should be the directory containing the gathered data.
This script expects a file 'full.json' to be present, and will create
a subdirectory 'plots' and save its plots there.

'''

import sys
import os
import json

from mbmlib import * # Local file with helper functions.

import numpy as np
import matplotlib
matplotlib.use('Agg') # For headless use.
import matplotlib.pyplot as plt


## INPUT
data_dir = sys.argv[1]

with open(os.path.join(data_dir, 'full.json')) as f:
    benchmarks = json.load(f)
benchmarks = list(benchmarks.items())


# PLOTTING
plots_dir = os.path.join(data_dir, 'plots')
os.makedirs(plots_dir, exist_ok=True)

keys = (('without-coalescing_without-reuse', 'disabled', 'red'),
        ('with-coalescing_with-reuse', 'enabled', 'blue'))

for benchmark_name, benchmark_info in benchmarks:
    for name, unit, val_func, kind_format in attributes:
        (kind, f) = val_func

        flat_results = []
        pdf_basename = '{}-{}.pdf'.format(benchmark_name, name.replace(' ', '_'))
        pdf_path = os.path.normpath(os.path.join(plots_dir, pdf_basename))
        if kind == 'datasets':
            for dataset_name, dataset_info in benchmark_info['datasets'].items():
                for key, short_name, color in keys:
                    value = f(dataset_info[key])
                    flat_results.append(('{}:\n{}'.format(short_name, dataset_name),
                                        value / 1000.0, color))
        elif kind == 'compilation':
            for key, short_name, color in keys:
                value = f(benchmark_info['compilation'][key])
                flat_results.append((short_name, value, color))

        os.makedirs(os.path.dirname(pdf_path), exist_ok=True)
        print('Plotting {} {} into {}.'.format(benchmark_name, name, pdf_path),
              file=sys.stderr)

        ind = np.arange(len(flat_results))
        width = 0.9

        maximum = max(map(lambda x: x[1], flat_results))

        fig, ax = plt.subplots()
        ax.set_ylim([0, maximum * 1.1])
        ax.set_title('{} of {}'.format(name, benchmark_name))
        ax.set_ylabel(unit)
        ax.set_xticks(ind)
        ax.set_xticklabels(list(map(lambda x: x[0], flat_results)))
        plt.tick_params(axis='x', which='major', labelsize=4)

        plt.bar(ind,
                list(map(lambda x: x[1], flat_results)),
                width,
                color=list(map(lambda x: x[2], flat_results)))

        plt.tight_layout(pad=0.4, w_pad=0.5, h_pad=1.0)

        plt.rc('text')
        plt.savefig(pdf_path, format='pdf')
        plt.close()
