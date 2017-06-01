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

# fix later
assert 'with.json' in json_paths
assert 'without.json' in json_paths

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
        total_alloc = {}
        for json_path, local_benchmarks in sorted(raw_results.items()):
            try:
                runtimes[json_path] = np.mean(local_benchmarks[benchmark_name]['datasets'][dataset_name]['runtimes'])
                total_alloc[json_path] = local_benchmarks[benchmark_name]['datasets'][dataset_name]['total_allocated']
            except TypeError:
                pass
        if 'without.json' in runtimes.keys() and 'with.json' in runtimes.keys():
            improvement_time = (runtimes['without.json'] - runtimes['with.json']) / runtimes['without.json']
            improvement_alloc = (total_alloc['without.json'] - total_alloc['with.json']) / total_alloc['without.json']
            datasets.append((dataset_name, runtimes, total_alloc, improvement_time, improvement_alloc))
    datasets.sort(key=lambda t: t[3:], reverse=True)
    benchmarks.append((benchmark_name, datasets))

def average_improvements(datasets):
    t, a = 0, 0
    for _, _, _, improvement_time, improvement_alloc in datasets:
        t += improvement_time
        a += improvement_alloc
    t /= len(datasets)
    a /= len(datasets)
    return (t, a)

benchmarks.sort(key=lambda t: average_improvements(t[1]), reverse=True)
# benchmarks.sort(key=lambda t: average_improvements(t[1])[0], reverse=True)
# benchmarks.sort(key=lambda t: average_improvements(t[1])[1], reverse=True)

def cut_desc(s):
    if s[0] == '#':
        return s.split(' ')[0]
    else:
        return os.path.split(s)[1]

for benchmark_name, datasets in benchmarks:
    print('# {}'.format(benchmark_name))
    t, a = average_improvements(datasets)
    print('Average improvement in runtime: {:.3f}'.format(t))
    print('Average improvement in total allocation: {:.3f}'.format(t))
    print('Datasets:')
    for dataset_name, runtimes, total_alloc, improvement_time, improvement_alloc in datasets:
        print('  ## Dataset: {}'.format(cut_desc(dataset_name)))
        print('  Runtimes: Without: {} us, with: {} us'.format(runtimes['without.json'], runtimes['with.json']))
        print('  Total allocation: Without: {} us, with: {} us'.format(total_alloc['without.json'], total_alloc['with.json']))
        print('  Improvement in runtime: {}'.format(improvement_time))
        print('  Improvement in total allocation: {}'.format(improvement_alloc))
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
    pdf_path = os.path.normpath(os.path.join(out_dir, benchmark_name + '-runtimes.pdf'))
    os.makedirs(os.path.dirname(pdf_path), exist_ok=True)
    flat_results = []
    for dataset_name, runtimes, total_alloc, _, _ in datasets:
        for json_path in ['without.json', 'with.json']:
            runtime = runtimes[json_path]
            color = json_colors[json_path]
            flat_results.append(('{}:\n{}'.format(short_path_name(json_path),
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


    pdf_path = os.path.normpath(os.path.join(out_dir, benchmark_name + '-allocations.pdf'))
    os.makedirs(os.path.dirname(pdf_path), exist_ok=True)
    flat_results = []
    for dataset_name, runtimes, total_alloc, _, _ in datasets:
        for json_path in ['without.json', 'with.json']:
            alloc = total_alloc[json_path]
            color = json_colors[json_path]
            flat_results.append(('{}:\n{}'.format(short_path_name(json_path),
                                                  cut_desc(dataset_name)),
                                 alloc, color))

    print('Plotting {} into {}.'.format(benchmark_name, pdf_path))

    ind = np.arange(len(flat_results))
    width = 0.35

    maximum = max(map(lambda x: x[1], flat_results))

    fig, ax = plt.subplots()
    ax.set_ylim([0, maximum * 1.1])
    ax.set_title('Cumulative allocations in {}'.format(benchmark_name))
    ax.set_ylabel('Bytes')
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
