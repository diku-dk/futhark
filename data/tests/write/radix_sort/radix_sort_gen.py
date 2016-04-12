#!/usr/bin/env python3

import sys
import random

n = int(sys.argv[1])

rands = [random.randint(0, 2**32 - 1) for i in range(n)]

with open('radix_sort_{}.in'.format(n), 'w') as f:
    print(rands, file=f)

rands.sort()

with open('radix_sort_{}.out'.format(n), 'w') as f:
    print(rands, file=f)
