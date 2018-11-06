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

You can also use ConfigV as an API from a Haskell program. For an example of this usage, see how the command line tool is built in the ```Executables``` directory, or inspect some of the tests in the ```Tests``` directory.

# Helpful Tips

## Inspecting Rules
The default location of the learned rules is ```cachedRules.json```
This can file be manually inspected as a sanity check. 
To pretty print this file, you use ```python -m json.tool cachedRules.json```

To see the files in a benchmark set use ```tail -n +1 benchmarks/MissingCSV/*```

## Support and Confidence 

Thresholds cannot be set using the command line tool. If you want to change the thresholds for the command line tool, you will need to change the code of the command line tool directly (Executables/Main.hs). All you need to do is pass in the Thresholds obeject that you prefer to use

When using the API version of ConfigV, you can pass in thresholds either as PercentageThresholds (the traditional support and confidence way), or as RawThresholds (which is specialized to the size of your training set). RawThresholds is a good way to build benchmark programs, but for real use cases, PercentageThresholds is the only practical choice.

## Publications

For more information on ConfigV and the theory behind how/why it works see:

- Synthesizing configuration file specifications with association rule learning, OOPSLA18, https://dl.acm.org/citation.cfm?doid=3133888
- Probabilistic Automated Language Learning for Configuration Files, CAV17, https://link.springer.com/chapter/10.1007%2F978-3-319-41540-6_5

## Maintainers

- Mark Santolucito
- Jurgen Cito 

Feel free to reach out if you have any questions about the tool or how to use it - happy to help!
