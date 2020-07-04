#!/usr/bin/env python
#
# See the impact of a Futhark commit compared to the previous one we
# have benchmarking for.

import sys
import subprocess
from urllib.request import urlopen
from urllib.error import HTTPError
import json

def url_for(backend, system, commit):
    return 'https://futhark-lang.org/benchmark-results/futhark-{}-{}-{}.json'.format(backend, system, commit)

def results_for_commit(backend, system, commit):
    try:
        url = url_for(backend, system, commit)
        print('Fetching {}...'.format(url))
        return json.loads(urlopen(url).read())
    except HTTPError:
        return None

def first_commit_with_results(backend, system, commits):
    for commit in commits:
        res = results_for_commit(backend, system, commit)
        if res:
            return commit, res

if __name__ == '__main__':
    backend, system, commit = sys.argv[1:]

    commits = subprocess.check_output(['git', 'rev-list', commit]).decode('utf-8').splitlines()

    now = results_for_commit(backend, system, commit)

    if not now:
        print('No results found')
        sys.exit(1)

    then_commit, then = first_commit_with_results(backend, system, commits[1:])

    print('Comparing {}'.format(commit))
    print('     with {}'.format(then_commit))

    # Hacky hacky...
    m = __import__('cmp-bench-json')
    m.compare(then, now)
