# ConfigV

A tool for learning rules about configuration langauges. 

ConfigV uses a generalization of Association Rule Learning to learn user-defined predicates over datasets of configuration files. You can think of this as data science meets programming languages meets systems.

## Basic Setup and Testing

To get ConfigV installed (assuming you have Haskell on your system)

```
cabal sandbox init
cabal install
```

Then to run the learning process on a small benchmark:

```
cabal test 
```

You can the check the log for details on the test run, and inspect the test code in the Tests directory.


## Basic Usage

For usage on your own dataset, you can use the command line tool, in a similar way to below. Your exact location of the executable may vary.

```
.cabal-sandbox/bin/ConfigV learn --learntarget "benchmarks/CSVTest/" --enableorder --enablemissing
```

You can also use ConfigV from a Haskell program. For this usage, see the Executables directory.

# Helpful Tips

The default location of the learned rules is ```cachedRules.json```
This can file be manually inspected as a sanity check. 
To pretty print this file, you use ```python -m json.tool cachedRules.json```

Support and Confidence If you want to see the effect of different support and confidence thresholds, simply edit the threshold settings in the Settings.hs file. The values are present to the values used in the evaluation in the paper. Your own input The best way to explore the tool at first is to keep the testing configuration and change values in the ’testLearn’ files or the ’user’ files. After adding more configuration settings (make up any keywords and values you
like) to ’testLearn’, you can inspect cachedRules.json to directly see what was learned. You can also change the support and confidence thresholds in src/Settings.hs to control when rules will be accepted (although on the small testLearn training set this isn’t very interesting).

## Publications

For more information on ConfigV and the theory behind how/why it works see:

- Probabilistic Automated Language Learning for Configuration Files, CAV17, https://link.springer.com/chapter/10.1007%2F978-3-319-41540-6_5
- Synthesizing configuration file specifications with association rule learning, OOPSLA18, https://dl.acm.org/citation.cfm?doid=3133888
