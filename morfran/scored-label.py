import sys
import csv
import numpy as np

import matplotlib.pyplot as plt

# Transformed the result of cerridwen eval "target,query,sim,true\n"
# into scored-label file "sim \t true \n"

# I/O
filename = sys.argv[1]
data = np.genfromtxt(filename, delimiter=',', encoding="utf8", dtype=None)

for i in data:
    print(i[2], end = '')
    print('\t', end = '')
    print(i[3])
