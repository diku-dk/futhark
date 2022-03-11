#! python3

import os
import sys
import subprocess

fut_file = "sample_programs/"+sys.argv[1]

subp = subprocess.run(['cabal', 'run', 'futhark', 'c', fut_file], capture_output=True)
subp.check_returncode()
out = subp.stderr
out = str(out).replace("\\n", "\n")


dg = out.split("digraph")[1:]

graphs = ["digraph" + x.split("\n}\n")[0] + "}" for x in dg ]

for i,G in enumerate(graphs):
    with open("scratch.txt", "+w") as f:
        f.write(G)
    img = subprocess.run(['dot', '-Tpng', 'scratch.txt'], capture_output=True).stdout
    with open(f'visgraph/{sys.argv[1][:-4]}_{i}.png', "w+b") as f:
        f.write(img)
