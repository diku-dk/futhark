#!/usr/bin/env python3

'''
Summarize the results you get from running './gather-data.sh' and then
'./merge-data.py'.

The sole argument should be the directory containing the gathered data.
This script expects a file 'full.json' to be present, and will create
a file 'summary.html' with a pretty-printed overview.
'''

import sys
import os
import json

from mbmlib import * # Local file with helper functions.

import numpy as np


## INPUT
data_dir = sys.argv[1]

with open(os.path.join(data_dir, 'full.json')) as f:
    benchmarks = json.load(f)
benchmarks = list(benchmarks.items())


# SUMMARY
attributes = (('average runtime',
               lambda d: d['average_runtime']),
              ('average peak memory usage',
               lambda d: np.mean(list(d['peak_memory_usages'].values()))),
              ('total cumulative allocations',
               lambda d: d['total_cumulative_allocations']),
              ('total cumulative frees',
               lambda d: d['total_cumulative_frees']))

with open(os.path.join(data_dir, 'summary.html'), 'w') as sys.stdout:
    print('''\
<!doctype html>
<html>
  <head>
    <title>Memory block merging results</title>
    <meta charset="UTF-8">
    <style type="text/css">
      body {
        width: 56em;
      }
    </style>
  </head>
<body>
''')
    print('<h1>Sorted after different improvements</h1>')
    for name, val_func in attributes:
        key_func = lambda i: average_improvement(i, val_func)
        temp = list(map(lambda b: (b[0], key_func(b[1])), benchmarks))
        temp.sort(key=lambda t: t[1], reverse=True)
        print('<h2>{}</h2>'.format(name))
        print('<p>Benchmarks sorted after the best average improvement across datasets in {} after enabling memory block merging:<br>(In-place lowering is enabled when memory block merging is disabled, and vice-versa.)</p>'.format(name)
              + '\n{}\n'.format('<br>'.join('{} ({})'.format(name, percentage_format(improvement_percent))
                                          for name, improvement_percent in temp)))

    print('<h1>Per-benchmark overview (sorted alphabetically)</h1>')
    benchmarks.sort()
    for benchmark_name, benchmark_info in benchmarks:
        print('<h2>{}</h2>'.format(benchmark_name))
        print('<p>(In-place lowering is enabled when memory block merging is disabled, and vice-versa.).</p>')

        for name, val_func in attributes:
            print('<p>Average improvement in {} after enabling memory block merging: {}</p>'.format(
                name, percentage_format(average_improvement(benchmark_info, val_func))))

        for dataset_name, dataset_info in benchmark_info['datasets'].items():
            print('<h3>dataset {}</h3>'.format(dataset_name))

            for name, val_func in attributes:
                print('<p>Improvement in {} after enabling memory block merging: {}</p>'.format(
                    name, percentage_format(dataset_improvement(dataset_info, val_func))))

            print('<h4>In-place lowering</h4>')
            print('<p>(Memory block merging always enabled.)</p>')
            before = dataset_info['with-in-place-lowering-with-memory-block-merging']
            after = dataset_info['without-in-place-lowering-with-memory-block-merging']

            for name, val_func in attributes:
                print('<p>Improvement in {} after disabling in-place-lowering: {}</p>'.format(
                    name, percentage_format(speedup_improvement(val_func(before), val_func(after)))))

    print('''\
  </body>
</html>
''')
