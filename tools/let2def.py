#!/usr/bin/env python3
#
# Usage: let2def.py prog.fut
#
# Replaces top-level 'let's with 'def'.

from subprocess import check_output
import sys
import re
import os

prog = sys.argv[1]
out = check_output(["futhark", "defs", prog]).decode("utf-8")

funlocs = set()
for row in out.split("\n"):
    m = re.match("value [^ ]+ ([^:]+):([0-9]+):([0-9]+)", row)
    if m and m[1] == prog:
        funlocs.add((int(m[2]), int(m[3])))

f = open(prog, "r+")
s = list(f.read())

line = 1
col = 0
n = len(s)
for i in range(n):
    if s[i] == "\n":
        line += 1
        col = 0
    elif s[i] == "\t":
        # Futhark lexer assumes tabwidth=8 for source positions.
        col += 1
        while col % 8 != 0:
            col += 1
    else:
        col += 1
    if s[i : i + 3] == list("let") and (line, col) in funlocs:
        s[i : i + 3] = "def"
f.seek(0)
f.truncate()
f.write("".join(s))
