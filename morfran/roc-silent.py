import sys
import csv
import numpy as np
from sklearn import metrics
import matplotlib.pyplot as plt

#  ROC for cerridwen
# adapted from examples on scikit-learn

# I/O
filename = sys.argv[1]
data = np.genfromtxt(filename, delimiter=',', encoding="utf8", dtype=None)
target = data[0][0]
simscores = [i[2] for i in data]
truescores = [i[3] for i in data]

# ROC
roc_auc = metrics.roc_auc_score(truescores, simscores)
print(str(roc_auc))
