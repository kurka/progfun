#!/usr/bin/env python

import numpy as np


smallns = [3, 3, 5, 10, 10, 20, 20, 50, 50, 100, 100]
bigns = [5000]*2 + [9999]*3

ns = smallns + bigns
print(len(ns))
for n in ns:
    print(n)
    # print(" ".join(map(str, np.sort(np.random.randint(1, n+1, n)))))
    print(" ".join(map(str, np.random.randint(1, n+1, n))))
