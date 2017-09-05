'''
Helper functions.
'''

import sys
import numpy as np


attributes = (
    # ('average runtime',
    #  'microseconds',
    #  lambda d: d['average_runtime']),
    ('average peak memory usage',
     'bytes',
     lambda d: np.mean(list(d['peak_memory_usages'].values()))),
    ('total cumulative allocations',
     'bytes',
     lambda d: d['total_cumulative_allocations']),
    ('total cumulative frees',
     'bytes',
     lambda d: d['total_cumulative_frees']),
)

def average_improvement(benchmark_info, val_func):
    datasets = benchmark_info['datasets'].values()
    return np.mean(
        [dataset_improvement(dataset_info, val_func)
         for dataset_info in datasets])

def dataset_improvement(dataset_info, val_func):
    return speedup_improvement(
        val_func(dataset_info['without-coalescing_without-reuse']),
        val_func(dataset_info['with-coalescing_with-reuse']))

def speedup_improvement(runtime_before, runtime_after):
    runtime_decrease = runtime_before - runtime_after
    return runtime_decrease / runtime_before

def percentage_format(n):
    s = '{:.3f}%'.format(n * 100)

    if n < 0:
        return '<span style="color: red;">{}</span>'.format(s)
    else:
        return '<span style="color: green;">{}</span>'.format(s)
