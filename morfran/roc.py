import sys
import csv
import numpy as np
from sklearn import metrics
import matplotlib.pyplot as plt

# Simple plot of ROC for cerridwen
# adapted from examples on scikit-learn

# I/O
filename = sys.argv[1]
data = np.genfromtxt(filename, delimiter=',', encoding="utf8", dtype=None)
target = data[0][0]
simscores = [i[2] for i in data]
truescores = [i[3] for i in data]

# ROC
fpr, tpr, thresholds = metrics.roc_curve(truescores, simscores)
roc_auc = metrics.roc_auc_score(truescores, simscores)

# PLOTLIB
plt.figure()
lw = 2
plt.plot(fpr, tpr, color='darkorange',
         lw=lw, label='ROC curve (area = %0.2f)' % roc_auc)
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC for ' + target)
plt.plot([0, 1], [0, 1], color='navy', lw=lw, linestyle='--')
plt.legend(loc="lower right")
plt.show()
