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
    print('<h1>Memory block merging results</h1>')
    print('<p>Improvements on less than ±0.1% are ignored (useful for memory use measurements, which either change or not change at all -- not so much for runtime measurements, which have noise).</p>')
    print('<p>Raw results: <a href="full.json">full.json</a>.</p>')
    print('<h2>Table of contents</h2>')
    print('<ul>')
    for name, _unit, _val_func in attributes:
        url = 'sorted_after_' + name.replace(' ', '_')
        title = 'Sorted after ' + name + ' improvement'
        print('<li><a href="#{}">{}</a></li>'.format(url, title))
    print('<li><a href="#sorted_after_poorest">Sorted after poorest average runtime reaction to disabling in-place lowering</a></li>')
    print('<li><a href="#sorted_after_num_coalescings">Sorted after number of coalescings</a></li>')
    print('<li><a href="#per_benchmark_overview">Per-benchmark overview</a></li>')
    print('<ul>')
    benchmarks.sort()
    for benchmark_name, _ in benchmarks:
        print('<li><a href="#{}">{}</a></li>'.format(benchmark_name, benchmark_name))
    print('</ul>')
    print('</ul>')

    print('<h1>Sorted after different improvements</h1>')
    print('<p>In-place lowering is enabled when memory block merging is disabled, and vice-versa.</p>')
    for name, _unit, val_func in attributes:
        key_func = lambda i: average_improvement(i, val_func)
        temp = list(map(lambda b: (b[0], key_func(b[1])), benchmarks))
        temp.sort(key=lambda t: t[1], reverse=True)
        print('<a name="{}"></a>'.format('sorted_after_'
                                         + name.replace(' ', '_')))
        print('<h2>Sorted after {} improvement</h2>'.format(name))
        print('<p>Benchmarks sorted after the best average improvement across datasets in {} after enabling memory block merging:</p>'.format(name)
              + '\n{}\n'.format('<br>'.join('<a href="#{}">{}</a> ({})'.format(name, name, percentage_format(improvement_percent))
                                          for name, improvement_percent in temp)))

    print('<a name="sorted_after_poorest"></a>')
    print('<h1>Sorted after poorest average runtime reaction to disabling in-place lowering</h1>')
    temp = []
    for benchmark_name, benchmark_info in benchmarks:
        datasets = list(benchmark_info['datasets'].items())
        datasets.sort()
        imps = []
        for dataset_name, dataset_info in datasets:
            before = dataset_info['with-in-place-lowering-with-memory-block-merging']
            after = dataset_info['without-in-place-lowering-with-memory-block-merging']
            imps.append(speedup_improvement(before['average_runtime'],
                                            after['average_runtime']))
        temp.append((benchmark_name, imps))
    temp.sort(key=lambda t: sum(t[1]))
    print('<br>'.join('<a href="#{}">{}</a> ({})'.format(
        name, name, ', '.join(percentage_format(imp) for imp in imps))
                      for name, imps in temp))

    print('<a name="sorted_after_num_coalescings"></a>')
    print('<h1>Sorted after number of coalescings</h1>')
    temp = []
    for benchmark_name, benchmark_info in benchmarks:
        num_coalescings = len(benchmark_info['compilation']['without-in-place-lowering'][0]['memory block merging'])
        temp.append((benchmark_name, num_coalescings))
    temp.sort(key=lambda t: t[1], reverse=True)
    print('<br>'.join('<a href="#{}">{}</a> ({})'.format(
        name, name, n) for name, n in temp))

    print('<a name="per_benchmark_overview"></a>')
    print('<h1>Per-benchmark overview (sorted alphabetically)</h1>')
    print('<p>For each benchmark, in-place lowering is enabled before turning on memory block merging, and disabled after turning on memory-block-merging.</p>')
    print('<p>In each "In-place lowering" subsection, memory block merging is always enabled, since the point is to see how different memory block merging acts without and with in-place lowering.</p>')
    benchmarks.sort()
    for benchmark_name, benchmark_info in benchmarks:
        print('<a name="{}"></h2>'.format(benchmark_name))
        print('<h2>{}</h2>'.format(benchmark_name))

        for name, _unit, val_func in attributes:
            imp = average_improvement(benchmark_info, val_func)
            if abs(imp) > improvement_difference_threshold_ignore:
                print('<p>Average improvement in {} after enabling memory block merging: {} (<a href="plots/{}-{}.pdf">see plot</a>)</p>'.format(
                    name, percentage_format(imp), benchmark_name, name.replace(' ', '_')))

        num_coalescings = len(benchmark_info['compilation']['without-in-place-lowering'][0]['memory block merging'])
        print('<p>Number of coalescings: {}</p>'.format(num_coalescings))

        datasets = list(benchmark_info['datasets'].items())
        datasets.sort()
        for dataset_name, dataset_info in datasets:
            print('<h3>dataset {}</h3>'.format(dataset_name))
            print('<p>Before: <pre>{}</pre></p>'.format(
                dataset_info['with-in-place-lowering-without-memory-block-merging']))
            print('<p>After: <pre>{}</pre></p>'.format(
                dataset_info['without-in-place-lowering-with-memory-block-merging']))

            for name, _unit, val_func in attributes:
                imp = dataset_improvement(dataset_info, val_func)
                if abs(imp) > improvement_difference_threshold_ignore:
                    print('<p>Improvement in {} after enabling memory block merging: {}</p>'.format(
                        name, percentage_format(imp)))

            print('<h4>In-place lowering</h4>')
            before = dataset_info['with-in-place-lowering-with-memory-block-merging']
            after = dataset_info['without-in-place-lowering-with-memory-block-merging']

            print('<p>Before: <pre>{}</pre></p>'.format(before))
            print('<p>After: <pre>{}</pre></p>'.format(after))

            for name, _unit, val_func in attributes:
                imp = speedup_improvement(val_func(before), val_func(after))
                if abs(imp) > improvement_difference_threshold_ignore:
                    print('<p>Improvement in {} after disabling in-place-lowering: {}</p>'.format(
                        name, percentage_format(imp)))

    print('''\
  </body>
</html>
''')
