#!/usr/bin/env python3
#
# See the impact of a Futhark commit compared to the previous one we
# have benchmarking for.

import sys
import subprocess
from urllib.request import urlopen
from urllib.error import HTTPError
import json
import tempfile
import os
import gzip

def url_for(backend, system, commit):
    return 'https://futhark-lang.org/benchmark-results/futhark-{}-{}-{}.json.gz'.format(backend, system, commit)

def results_for_commit(backend, system, commit):
    try:
        url = url_for(backend, system, commit)
        print('Fetching {}...'.format(url))
        return json.loads(gzip.decompress(urlopen(url).read()))
    except HTTPError:
        return None

def first_commit_with_results(backend, system, commits):
    for commit in commits:
        res = results_for_commit(backend, system, commit)
        if res:
            return commit, res

def find_commits(start):
    return subprocess.check_output(['git', 'rev-list', start]).decode('utf-8').splitlines()

if __name__ == '__main__':
    backend, system, commit = sys.argv[1:4]

    now = results_for_commit(backend, system, commit)

    if not now:
        print('No results found')
        sys.exit(1)

    if len(sys.argv) == 5:
        commits = find_commits(sys.argv[4])
    else:
        commits = find_commits(commit)[1:]

    then_commit, then = first_commit_with_results(backend, system, commits[1:])

    print('Comparing {}'.format(commit))
    print('     with {}'.format(then_commit))

    with tempfile.NamedTemporaryFile(prefix=commit, mode='w') as now_file:
        with tempfile.NamedTemporaryFile(prefix=then_commit, mode='w') as then_file:
            json.dump(now, now_file)
            json.dump(then, then_file)
            now_file.flush()
            then_file.flush()
            os.system('tools/cmp-bench-json.py {} {}'.format(then_file.name, now_file.name))
