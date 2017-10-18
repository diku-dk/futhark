#!/usr/bin/env python

import numpy as np
import sys
import json
import re
import subprocess
import datetime

import matplotlib

matplotlib.use('Agg') # For headless use

import matplotlib.pyplot as plt
from matplotlib.ticker import FormatStrFormatter

import os

_, results_dir, machine, benchmark, plotfile = sys.argv

def compute_commit_timestamps():
    log = subprocess.check_output(['git', 'log', '--pretty=format:%H %at'])
    result = {}
    for line in log.split('\n'):
        commit,timestamp = line.split(' ')
        result[commit] = int(timestamp)
    return result

def remove_nones(l):
    return filter(lambda x: x is not None, l)

commit_timestamps = compute_commit_timestamps()

def cut_desc(s):
    if s[0] == '#':
        return s.split(' ')[0]
    else:
        return s

def extract_result(filename):
    match = re.search('^{}-([a-f0-9]+).json$'.format(machine), filename)
    if match:
        commit=match.group(1)
        results = json.load(open(results_dir + '/' + filename))

        try:
            benchmark_results = results['futhark-benchmarks/'+benchmark]
            def get_runtime(r):
                for dataset in r:
                    if type(r[dataset]) is dict:
                        return np.mean(r[dataset]['runtimes'])
            runtimes={}
            for dataset in benchmark_results['datasets']:
                if type(benchmark_results['datasets'][dataset]) is dict:
                    runtimes[cut_desc(dataset)] = np.mean(benchmark_results['datasets'][dataset]['runtimes'])
            return {'timestamp': commit_timestamps[commit],
                    'commit': commit,
                    'runtimes': runtimes}
        except:
            return None

results = remove_nones(map (extract_result,
                            os.listdir(results_dir)))
results.sort(key=lambda x: x['timestamp'])

if len(results) == 0:
    sys.exit('No results found for benchmark {}.'.format(benchmark))

best = {}

def from_unixtime(timestamp):
    return datetime.datetime.fromtimestamp(timestamp).strftime('%Y-%m-%d')

for r in results:
    time = from_unixtime(r['timestamp'])
    for dataset in r['runtimes']:
        if dataset not in best or r['runtimes'][dataset] < best[dataset]['runtime']:
            best[dataset] = {'runtime': r['runtimes'][dataset],
                             'timestamp': r['timestamp'],
                             'commit': r['commit'] }
        print r['commit'], dataset, time, r['runtimes'][dataset]

for dataset in sorted(best.keys()):
    best_time = from_unixtime(best[dataset]['timestamp'])
    print 'Dataset {} best: {} {} {}'.format(dataset, best_time, best[dataset]['commit'], best[dataset]['runtime'])

def make_xticks(results):
    times = np.array(map(lambda x: from_unixtime(x['timestamp']), results))
    return times[np.arange(0,len(times),len(times)/10)]

fig, ax = plt.subplots()
ax.set_title(benchmark)
ax.set_ylabel('Slowdown compared to fastest')

for dataset in sorted(best.keys()):
    best_runtime=best[dataset]['runtime']
    xs=[]
    ys=[]
    i = 0
    for r in results:
        if dataset in r['runtimes']:
            xs += [i]
            ys += [r['runtimes'][dataset]/best_runtime]
        i += 1
    ax.plot(xs, ys, label=dataset)

handles, labels = ax.get_legend_handles_labels()
ax.legend(handles, labels)

grey='#aaaaaa'
xticks=make_xticks(results)

ax.set_yscale('log')
ax.set_ylim(ymin=0.9,ymax=3)
ax.yaxis.grid(color=grey,zorder=0)
ax.yaxis.set_major_formatter(FormatStrFormatter('%.2f'))
ax.yaxis.set_minor_formatter(FormatStrFormatter(''))
ax.set_yticks(np.arange(0.9,3,0.1))

ax.set_xticks(1+np.arange(len(xticks))*(len(results)/10))
ax.set_xticklabels(xticks, rotation=-45)

plt.rc('text')
plt.savefig(plotfile, bbox_inches='tight')
