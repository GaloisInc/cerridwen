import sys
import csv
import numpy as np
import subprocess
# Script to automate the evaluation of cerridwen similarity score over multiple code samples
# It expects as input:
# 1) a CSV file where each line has the form:
#    NAME, TARGET, POSITIVE, NEGATIVE, CORPUS, BOUND
# where bound is 0 to run the evaluation with Gitz similarity score
# For each line, it performs the following:
# 1) cabal run cerridwen -- eval -t TARGET  -p POSITIVE -n NEGATIVE -e -s CORPUS --output NAME.csv --bound N
# 1* if TARGET, POSITIVE, NEGATIVE, CORPUS are exactly "X", use the value in the first row
# 1** if NAME is exactly "X", use the value in the first row + bound 
# 2) generate the file name.scored-label using morfran.scored-label.py
# 3) generate the file name.curve and compute the croc score using croc-curve
# 4) compute the ROC score using morfran/roc.py
# 5) Outputs NAME, ROC, CROC
default = "X"


# I/O
filename = sys.argv[1]
data = np.genfromtxt(filename, delimiter=',', encoding="utf8", dtype=None)
d_name = data[0][0]
d_target = data[0][1]
d_positive = data[0][2]
d_negative = data[0][3]
d_corpus = data[0][4]


for row in data:
    target = d_target if (row[1] == default) else row[1]
    positive = d_positive if (row[2] == default) else row[2]
    negative = d_negative if (row[3] == default) else row[3]
    corpus = d_corpus if (row[4] == default) else row[4]
    bound = row[5]
    name = (d_name + str(bound)) if (row[0] == default) else row[0]
    
    cerridwenCall = "cabal run cerridwen -- eval -t " + target + " -p " + positive + " -n " + negative + " -e -s " + corpus + " --output " + name + ".csv -c --bound " + str(bound)
    scoredlabelCall = "python3 morfran/scored-label.py " + name + ".csv > " + name + ".scored-label"
    rocCall = "python3 morfran/roc-silent.py " + name + ".csv"
    crocCall = "croc-curve < " + name + ".scored-label > " + name + ".curve"
    # 1
    subprocess.check_output(cerridwenCall,  shell=True)
    # 2
    subprocess.check_output(scoredlabelCall,  shell=True)
    # 3
    crocResult = subprocess.check_output(crocCall, shell=True, stderr=subprocess.STDOUT, text=True).split("=")
    # 4
    rocResult = subprocess.check_output(rocCall, shell=True, text=True)
    # 5
    print(name + ", " + rocResult.strip() + ", " + crocResult[1].strip())
    
    
    
