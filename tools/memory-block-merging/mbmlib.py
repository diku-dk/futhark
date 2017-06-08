'''
Helper functions.
'''

import numpy as np


def average_improvement(benchmark_info, val_func):
    datasets = benchmark_info['datasets'].values()
    return np.mean(
        [dataset_improvement(dataset_info, val_func)
         for dataset_info in datasets])

def dataset_improvement(dataset_info, val_func):
    return speedup_improvement(
        val_func(dataset_info['with-in-place-lowering-without-memory-block-merging']),
        val_func(dataset_info['without-in-place-lowering-with-memory-block-merging']))

def speedup_improvement(runtime_before, runtime_after):
    runtime_decrease = runtime_before - runtime_after
    return runtime_decrease / runtime_before

def percentage_format(n):
    s = '{:.3f}%'.format(n * 100)

    if n < 0:
        return '<span style="color: red;">{}</span>'.format(s)
    else:
        return '<span style="color: green;">{}</span>'.format(s)
