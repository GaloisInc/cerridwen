Cerridwen is (C) 2022 Galois, Inc., and distributed under a standard, three-clause BSD license. Please see the file LICENSE, distributed with this software, for specific terms and conditions.

# Cerridwen
This repository implements a tool to generate and weight hash of binaries in order to be able to quantify similarities between binaries. Inspiration for the algorithm and the unbounded similarity score are taken from "Similarity of Binaries through re-Optimization" (David, Partush and Yahav, PLDI 2017).

# Dependencies
This repository installs most of its code dependencies as git submodules. However, it assumes that Cabal (https://www.haskell.org/cabal/) and Cargo (https://doc.rust-lang.org/cargo/) are installed.

# Building
	$ git submodule update --init
	$ cp cabal.project.dist cabal.project
	$ cabal configure
	$ cabal build
	
or run script `INSTALL.sh`

# Workflow
In this section, we describe in more detail an example of a workflow for using this tool.

## Generating a corpus
Cerridwen uses a corpus of binaries to approximate the frequency of occurrence of a given strand in "code at large". For example, one could use the collection of benignware provided at https://github.com/nimrodpar/Labeled-Elfs as a corpus.
We summarize the corpus in a corpus summary capturing the frequency of each strand in the corpus, in addition to the number of unique strands encountered. To generate a corpus summary, first collect a large number of binaries and place them in a folder (for this example, we assume `cerridwen/examples/corpus` is the folder containing the binaries). Then, run the corpus generation command: 
```
$ cerridwen gen --corpus "examples/corpus/*.o" --summary examples/corpus-summary
```
This command will generate a corpus from the binaries matched by the Filepath `examples/corpus/*.o` and save it as `examples/corpus-summary`.

## Evaluating the similarity of a target in a query
We can then use Cerridwen and our generated corpus summary to compute a similarity score:
```
$ cerridwen sim --target example/target.o --query examples/query.o --summary examples/corpus-summary
```
The output is a similarity score where a higher score means a higher change that `query.o` contains code that is similar to `target.o`. 

# Running Cerridwen
```
	$ cerridwen COMMAND [OPTIONS]
```
  Use 
```
    $ cerridwen --help 
```
  For a list of available commands and their associated options

## `sim`
  Compute the similarity between a target and a query. 
  * input: a target and query ELF file and a corpus (either as a summary with `--summary` or a pattern of executable to generate the corpus with `--corpus`)
  * output: a similarity score.
  
```
$ cerridwen sim --target ../esh-dataset-1523/Positive/clang.3.5_openssl.1.0.1f_d1_both.o__dtls1_process_heartbeat.o --query ../esh-dataset-1523/Positive/clang.3.5_openssl.1.0.1g_d1_both.o__dtls1_process_heartbeat.o --summary examples/Random-Labeled-Elfs/corpus-all --bound 1000
```

## `hash`
  Find and hash strands reachable from the entry points in an ELF file
  * input: an ELF file to explore, a bound or threshold and a corpus summary if compressed similarity is used
  * output: a list of strand hashes
  
```
$ cerridwen hash -i examples/Labeled-Elfs/x86__64__lsb__unix-system-v__clang-3.8.0__Os__no-obf__unstripped__acpid-2.0.31__sock.o
$ cerridwen hash -i examples/Labeled-Elfs/x86__64__lsb__unix-system-v__clang-3.8.0__Os__no-obf__unstripped__acpid-2.0.31__sock.o  -h 20 -summary examples/Random-Labeled-Elfs/corpus-all
```

## `gen`
  Generate a corpus summary (map from strand hashes to their frequency in the corpus) from a collection of executables
  * input: a POSIX matching pattern for ELF executables to include in the corpus
  * output: a corpus summary saved at the provided location (`--summary`)

```
$ cerridwen gen --corpus "examples/macaw-x86-symbolic/*.exe" --summary examples/macaw-x86-symbolic/corpus-5 --opt 5
```

## `eval`
  Run `sim` with the same target over two lists of queries, identified as positive and negative examples
  * input: an ELF target (`--target`), a POSIX matching pattern for queries that are known to be similar to the target (`--positive`), a POSIX matching pattern for queries that are known to be disimilar to the target (`--negative`), a corpus summary (`--summary`)
  * output: a CSV file (at --output) where each line is the result of `sim` in the following format: `[TARGET], [QUERY], [SIMILARITY], [POSITIVE?]`

```
$ cerridwen eval -t examples/macaw-x86-symbolic/saturate-add.unopt.exe -p "examples/macaw-x86-symbolic/saturate-add.*.exe" -n "examples/macaw-x86-symbolic/*.exe" -e -s examples/macaw-x86-symbolic/corpus --output examples/macaw-x86-symbolic/simscores.csv
```

## `merge`
  Merge unoptimized corpus summaries
  * input: Any number of corpus summaries
  * output: A single corpus summary saved at the provided location (`--output`)

```
$ cerridwen gen --corpus "examples/Random-Labeled-Elfs/dir_001/*" --summary examples/Random-Labeled-Elfs/corpus001un --opt 0
...
$ cerridwen merge -i examples/Random-Labeled-Elfs/corpus001un -i examples/Random-Labeled-Elfs/corpus002un -i examples/Random-Labeled-Elfs/corpus003un -i examples/Random-Labeled-Elfs/corpus004un -i examples/Random-Labeled-Elfs/corpus005un -o examples/Random-Labeled-Elfs/corpusmerged
```


## `print`
  Display (or print to file) a corpus summary as CSV
  * input: a corpus summary
  * output: a CVS file with the frequencies of each strand hashes in the corpus in the following format:  `[HASH], [FREQUENCY]`

```
$ cerridwen print -s examples/macaw-x86-symbolic/corpus-5
```

# Other scripts (Morfran)
A few python scripts are included in the `morfran` folder in order to interface with numpy and gitz:
## `morfran/roc.py`
  Compute and display the ROC for the output of `cerridwen eval`
```
$ python morfran/roc.py eval.csv
```


## `morfran/scored-label.py INPUT`
  Transform the output of `cerridwen eval` into scored-label file which are input to gitz

```
$ python morfran/scored-label.py eval.csv > eval.scored-label
```

## `morfran/evaluate.py INPUT`

  Repeditively calls `cerridwen eval` followed by `morfran/roc.py` and `gitz-croc` on each line of the input CSV, formated as 
  `NAME, TARGET, POSITIVE, NEGATIVE, CORPUS, BOUND`. The output is a CSV with format `NAME, ROC, CROC` where each line corresponds to the line sharing the same `name` in the input file.

```
$ python morfran/evaluate.py example/test-evaluate.csv > result.csv
```

 X can be used in place of `TARGET`, `POSITIVE`, `NEGATIVE` and `CORPUS` to use instead the value in the first row.
 X can be used in place of `NAME` to use as `NAME` the `BOUND` appended to first row's `NAME`.

## `morfran/evaluateT.py INPUT`
  Same as `morfran/evaluate.py` except the input is formated as
`NAME, TARGET, POSITIVE, NEGATIVE, CORPUS, THRESHOLD`, i.e. using a threshold to set the bound for strand significance.

 X can be used in place of `TARGET`, `POSITIVE`, `NEGATIVE` and `CORPUS` to use instead the value in the first row.
 X can be used in place of `NAME` to use as `NAME` the `THRESHOLD` appended to first row's `NAME`.

## `morfran/evaluateA.py INPUT OUTPUT`
  
   Repeditively calls `cerridwen eval -a` on each line of the input CSV, formated as
   `NAME, TARGET,POSITIVE, NEGATIVE, CORPUS, BOUND, ACCURACY`, saving the results in the csv file `OUTPUT` as
   `ACCURARY, Bound, #TP, #FP, #TN, #FN`
   
 X can be used in place of `TARGET`, `POSITIVE`, `NEGATIVE`, `CORPUS` and `BOUND` to use instead the value in the first row.
 X can be used in place of `NAME` to use as `NAME` the `ACCURACY` appended to first row's `NAME`.

# Acknowledgements

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) and Space and Naval Warfare
Systems Center, Pacific (SSC Pacific) under Contract No. N66001-15-C-4070. Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of DARPA or SSC Pacific.
