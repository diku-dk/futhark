#!/usr/bin/env python3

import sys
import os
import json

import numpy as np
import matplotlib
matplotlib.use('Agg') # For headless use.
import matplotlib.pyplot as plt


out_dir = sys.argv[1]
json_paths = sys.argv[2:]

raw_results = {}
for json_path in json_paths:
    with open(json_path) as f:
        res = json.load(f)
    raw_results[json_path] = res

benchmark_names = []
for local_benchmarks in raw_results.values():
    for benchmark_name in local_benchmarks.keys():
        benchmark_names.append(benchmark_name)
benchmark_names = list(set(benchmark_names))

dataset_names = {}
for benchmark_name in benchmark_names:
    names = []
    for local_benchmarks in raw_results.values():
        for dataset_name in local_benchmarks[benchmark_name]['datasets']:
            names.append(dataset_name)
    dataset_names[benchmark_name] = list(set(names))

benchmarks = []
for benchmark_name in benchmark_names:
    datasets = []
    for dataset_name in sorted(dataset_names[benchmark_name]):
        runtimes = {}
        for json_path, local_benchmarks in sorted(raw_results.items()):
            runtimes[json_path] = np.mean(local_benchmarks[benchmark_name]['datasets'][dataset_name]['runtimes'])
        improvement = (runtimes['without.json'] - runtimes['with.json']) / runtimes['without.json']
        datasets.append((dataset_name, runtimes, improvement))
    datasets.sort(key=lambda t: t[2], reverse=True)
    benchmarks.append((benchmark_name, datasets))

def average_improvement(datasets):
    i = 0
    for _, _, improvement in datasets:
        i += improvement
    i /= len(datasets)
    return i

benchmarks.sort(key=lambda t: average_improvement(t[1]), reverse=True)

def cut_desc(s):
    if s[0] == '#':
        return s.split(' ')[0]
    else:
        return s

for benchmark_name, datasets in benchmarks:
    print('# {}'.format(benchmark_name))
    print('Average improvement: {:.3f}'.format(average_improvement(datasets)))
    print('Datasets:')
    for dataset_name, runtimes, improvement in datasets:
        print('  ## Dataset: {}'.format(cut_desc(dataset_name)))
        print('  Runtimes: Without: {} us, with: {} us'.format(runtimes['without.json'], runtimes['with.json']))
        print('  Improvement: {}'.format(improvement))
    print('')


# Plot!

os.makedirs(out_dir, exist_ok=True)

def forever_repeat(xs):
    while True:
        for x in xs:
            yield x

colors = forever_repeat(['red', 'blue', 'grey'])

json_colors = {}
for json_path, color in zip(json_paths, colors):
    json_colors[json_path] = color

def short_path_name(path):
    try:
        return {'with.json': 'enabled',
                'without.json': 'disabled'}[path]
    except KeyError:
        return path

for benchmark_name, datasets in benchmarks:
    pdf_path = os.path.normpath(os.path.join(out_dir, benchmark_name + '.pdf'))
    os.makedirs(os.path.dirname(pdf_path), exist_ok=True)
    flat_results = []
    for dataset_name, results, _ in datasets:
        for json_path in ['without.json', 'with.json']:
            runtime = results[json_path]
            color = json_colors[json_path]
            flat_results.append(('{}: {}'.format(short_path_name(json_path),
                                                 cut_desc(dataset_name)),
                                 runtime, color))

    print('Plotting {} into {}.'.format(benchmark_name, pdf_path))

    ind = np.arange(len(flat_results))
    width = 0.35

    maximum = max(map(lambda x: x[1], flat_results))

    fig, ax = plt.subplots()
    ax.set_ylim([0, maximum * 1.1])
    ax.set_title('Runtimes of {}'.format(benchmark_name))
    ax.set_ylabel('Microseconds')
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
