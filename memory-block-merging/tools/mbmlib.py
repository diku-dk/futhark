'''
Helper functions.
'''

import sys
import numpy as np


attributes = (
    # ('average runtime',
    #  'microseconds', ('datasets',
    #                   lambda d: d['average_runtime'])),
    ('average peak memory usage',
     'bytes', ('datasets',
               lambda d: np.mean(list(d['peak_memory_usages'].values())))),
    ('total cumulative allocations',
     'bytes', ('datasets',
               lambda d: d['total_cumulative_allocations'])),
    ('total cumulative frees',
     'bytes', ('datasets',
               lambda d: d['total_cumulative_frees'])),
    ('number of coalescings',
     'amount', ('compilation',
                lambda d: len(list(set(xs[2] for xs in d
                                       if xs[0] == 'coalescing'))))),
    ('number of reuses',
     'amount', ('compilation',
                lambda d: len(list(set(xs[2] for xs in d
                                       if xs[0] == 'reuse'))))),
)

def average_improvement(benchmark_info, val_func):
    (kind, f) = val_func
    if kind == 'datasets':
        datasets = benchmark_info['datasets'].values()
        return np.mean(
            [base_improvement(dataset_info, f)
             for dataset_info in datasets])
    elif kind == 'compilation':
        compilation_info = benchmark_info['compilation']
        return base_improvement(compilation_info, f)

def base_improvement(dataset_info, val_func):
    return improvement(
        val_func(dataset_info['without-coalescing_without-reuse']),
        val_func(dataset_info['with-coalescing_with-reuse']))

def improvement(before, after):
    decrease = before - after
    try:
        return decrease / before
    except ZeroDivisionError:
        return after - before # Not percentages, fix this

def percentage_format(n):
    s = '{:.3f}%'.format(n * 100)

    if n < 0:
        return '<span style="color: red;">{}</span>'.format(s)
    else:
        return '<span style="color: green;">{}</span>'.format(s)
