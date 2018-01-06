#!/usr/bin/env python3

'''
Summarize the results you get from running './gather-data-coarse.sh' and
then './merge-data.py'.

The sole argument should be the directory containing the gathered data.
This script expects a file 'full.json' to be present, and will create
a file 'summary.html' with a pretty-printed overview.
'''

import sys
import os
import json

from mbmlib import * # Local file with helper functions.

import numpy as np


# SETTINGS
improvement_difference_threshold_ignore = 0.001 # 0.1%


# INPUT
data_dir = sys.argv[1]

with open(os.path.join(data_dir, 'full.json')) as f:
    benchmarks = json.load(f)
benchmarks = list(benchmarks.items())


# SUMMARY
with open(os.path.join(data_dir, 'summary.html'), 'w') as sys.stdout:
    print('''\
<!doctype html>
<html>
  <head>
    <title>Memory block merging results (coarse)</title>
    <meta charset="UTF-8">
    <style type="text/css">
      body {
        width: 56em;
      }
    </style>
  </head>
<body>
''')
    print('<h1>Memory block merging results (coarse)</h1>')
    print('<p>Raw results: <a href="full.json">full.json</a>.</p>')
    print('<h2>Table of contents</h2>')
    print('<ul>')
    for name, _unit, _val_func, _kind_format in attributes:
        url = 'sorted_after_' + name.replace(' ', '_')
        title = 'Sorted after ' + name + ' improvement'
        print('<li><a href="#{}">{}</a></li>'.format(url, title))
    print('<li><a href="#per_benchmark_overview">Per-benchmark overview</a></li>')
    print('<ul>')
    benchmarks.sort()
    for benchmark_name, _ in benchmarks:
        print('<li><a href="#{}">{}</a></li>'.format(benchmark_name, benchmark_name))
    print('</ul>')
    print('</ul>')

    for name, _unit, val_func, _kind_format in attributes:
        key_func = lambda i: average_improvement(i, val_func)
        temp = list(map(lambda b: (b[0], key_func(b[1])), benchmarks))
        temp.sort(key=lambda t: t[1], reverse=True)
        print('<a name="{}"></a>'.format('sorted_after_'
                                         + name.replace(' ', '_')))
        print('<h2>Sorted after {} improvement</h2>'.format(name))
        print('<p>Benchmarks sorted after the best average improvement across datasets in {} after enabling memory block merging:</p>'.format(name)
              + '\n{}\n'.format('<br>'.join('<a href="#{}">{}</a> ({})'.format(name, name, percentage_format(improvement_percent))
                                          for name, improvement_percent in temp)))

    print('<a name="per_benchmark_overview"></a>')
    print('<h1>Per-benchmark overview (sorted alphabetically)</h1>')
    print('<p>Before: In-place lowering enabled.  Both memory coalescing and reuse <b>disabled</b>.</p>')
    print('<p>After: In-place lowering disabled.  Both memory coalescing and reuse <b>enabled</b>.</p>')
    benchmarks.sort()
    for benchmark_name, benchmark_info in benchmarks:
        print('<a name="{}"></h2>'.format(benchmark_name))
        print('<h2>{}</h2>'.format(benchmark_name))

        for name, _unit, val_func, kind_format in attributes:
            imp = average_improvement(benchmark_info, val_func)
            if abs(imp) > improvement_difference_threshold_ignore:
                print('<p>Average improvement in {} after enabling both memory block merging passes: {} (<a href="plots/{}-{}.pdf">see plot</a>)</p>'.format(
                    name, percentage_format(imp), benchmark_name, name.replace(' ', '_')))

        datasets = list(benchmark_info['datasets'].items())
        datasets.sort()
        for dataset_name, dataset_info in datasets:
            print('<h3>dataset {}</h3>'.format(dataset_name))
            for k, v in sorted(dataset_info.items(), reverse=True):
                print('<p>{}: <pre>{}</pre></p>'.format(
                    k.replace('-', ' ').replace('_', ', '), v))

            for name, _unit, val_func, kind_format in attributes:
                (kind, f) = val_func
                if kind == 'datasets':
                    imp = base_improvement(dataset_info, f, 'percentage')
                    if abs(imp) > improvement_difference_threshold_ignore:
                        print('<p>Improvement in {}: {}</p>'.format(
                            name, percentage_format(imp)))

    print('''\
  </body>
</html>
''')
