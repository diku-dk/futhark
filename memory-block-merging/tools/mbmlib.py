'''
Helper functions.
'''

import sys
import numpy as np


def average_improvement(benchmark_info, val_func):
    (kind, f) = val_func
    if kind == 'datasets':
        datasets = benchmark_info['datasets'].values()
        return np.mean(
            [base_improvement(dataset_info, f, 'percentage')
             for dataset_info in datasets])
    elif kind == 'compilation':
        compilation_info = benchmark_info['compilation']
        return base_improvement(compilation_info, f, 'scalar')

def base_improvement(info, f, how):
    return improvement(
        f(info['without-coalescing_without-reuse']),
        f(info['with-coalescing_with-reuse']),
        how)

def improvement(before, after, how):
    if how == 'percentage':
        decrease = before - after
        return decrease / before
    elif how == 'scalar':
        return after - before

def percentage_format(n):
    s = '{:.3f}%'.format(n * 100)

    if n < 0:
        return '<span style="color: red;">{}</span>'.format(s)
    else:
        return '<span style="color: green;">{}</span>'.format(s)

attributes = (
    ('average runtime',
     'milliseconds', ('datasets',
                      lambda d: d['average_runtime']),
     percentage_format),
    ('average peak memory usage',
     'kilobytes', ('datasets',
               lambda d: np.mean(list(d['peak_memory_usages'].values()))),
     percentage_format),
    ('total cumulative allocations',
     'kilobytes', ('datasets',
               lambda d: d['total_cumulative_allocations']),
     percentage_format),
    ('total cumulative frees',
     'kilobytes', ('datasets',
               lambda d: d['total_cumulative_frees']),
     percentage_format),
    ('number of coalescings',
     'amount', ('compilation',
                lambda d: len(list(set(xs[2] for xs in d
                                       if xs[0] == 'coalescing')))),
     lambda x: x),
    ('number of reuses',
     'amount', ('compilation',
                lambda d: len(list(set(xs[2] for xs in d
                                       if xs[0] == 'reuse')))),
     lambda x: x),
)
