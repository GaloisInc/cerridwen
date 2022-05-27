import sys
import csv
import numpy as np
import subprocess
# Script to automate the evaluation of cerridwen similarity score over multiple code samples and threshold
# It expects as input:
# 1) a CSV file where each line has the form:
#    NAME, TARGET, POSITIVE, NEGATIVE, CORPUS, BOUND, ACCURACY
# 2) an output file to put the result
# where bound is 0 to run the evaluation with Gitz similarity score
# For each line, it performs the following:
# 1) cabal run cerridwen -- eval -t TARGET  -p POSITIVE -n NEGATIVE -e -s CORPUS --output NAME.csv --bound N --accuracy A
# 1* if TARGET, POSITIVE, NEGATIVE, CORPUS, BOUND are exactly "X", use the value in the first row
# 1** if NAME is exactly "X", use the value in the first row + accuracy 
# 2) Generate a CSV file in FILE with ACCURACY, Bound, #TP, #FP, #TN, #FN
default = "X"

# I/O
filename = sys.argv[1]
outputname = sys.argv[2]
data = np.genfromtxt(filename, delimiter=',', encoding="utf8", dtype=None)
d_name = data[0][0]
d_target = data[0][1]
d_positive = data[0][2]
d_negative = data[0][3]
d_corpus = data[0][4]
d_bound = data[0][5]

for row in data:
    target = d_target if (row[1] == default) else row[1]
    positive = d_positive if (row[2] == default) else row[2]
    negative = d_negative if (row[3] == default) else row[3]
    corpus = d_corpus if (row[4] == default) else row[4]
    bound = d_bound if (row[5] == default) else row[5]
    accuracy = row[6]

    cerridwenCall = "cabal run cerridwen -- eval --target " + target + " -p " + positive + " -n " + negative + " -e -s " + corpus + " -c --bound " + str(bound) + " -a "+ str(accuracy) + " --aoutput " + outputname
    subprocess.check_output(cerridwenCall,  shell=True)
    
    
    
