[![Build Status](https://travis-ci.com/ConfigV/ConfigV.svg?branch=master)](https://travis-ci.com/ConfigV/ConfigV)

# ConfigV

A tool for learning rules about configuration languages. 

ConfigV uses a generalization of Association Rule Learning to learn user-defined predicates over datasets of configuration files. You can think of this as data science meets programming languages meets systems.

## Basic Setup and Testing

To get ConfigV installed (assuming you have Haskell on your system), follow these steps.
The first installation will take a bit of time to download packages, but will be quick(er) after that.

```
git clone https://github.com/ConfigV/ConfigV
cd ConfigV
cabal build
```

This assumes GHC 8.4.3 or 8.6.3 and cabal 3.0.0.
If you are on Ubuntu, these can be easily installed here https://launchpad.net/~hvr/+archive/ubuntu/ghc
If you want to run the test suites you need to install the ghv-$VERS-prof package from the hvr repo as well

## Basic Usage

For usage on your own dataset, you can use the command line tool, in a similar way to below. Your exact location of the executable may vary.

```
cabal run ConfigVtool -- learn --learntarget "Datasets/benchmarks/CSVTest/" --enableorder 
```

You can also use ConfigV as an API from a Haskell program. For an example of this usage, see how the command line tool is built in the ```Executables``` directory, or inspect some of the tests in the ```Tests``` directory.

## CloudFormation Templates

To build a datset of Amazon CFN templates, we need to first preprocess the templates into a form ConfigV can handle (key-value pairs).
First collect a set of json CFN templates into a directory.
Note, yaml is not supported at the moment, though I was able to use the tool ```yq``` at one point for fairly good conversion.
To then convert the json to CSV for ConfigV, use the ```preprocessCFN.py``` file. You just need to change the constants in the .py code, then run with ```python preprocessCFN.py```.
This might not work for every file (some templates are malformed), so these templates might need to be discarded.

To run the learning process on the Amazon CFN templates, check the settings in ```Executables/AmazonCFN.hs```, then run:

```
cabal run AmazonCFN
```

## Datasets

Please feel free to add or modify datasets. All datasets should remain in the Datasets directory.

# Helpful Tips

## Inspecting Rules
The default location of the learned rules is ```cachedRules.json```
This can file be manually inspected as a sanity check. 
To pretty print this file, you use ```python -m json.tool cachedRules.json```

To see the files in a benchmark set use ```tail -n +1 Datasets/benchmarks/MissingCSV/*```

## Support and Confidence 

Thresholds cannot be set using the command line tool. If you want to change the thresholds for the command line tool, you will need to change the code of the command line tool directly (Executables/Main.hs). All you need to do is pass in the Thresholds obeject that you prefer to use

When using the API version of ConfigV, you can pass in thresholds either as PercentageThresholds (the traditional support and confidence way), or as RawThresholds (which is specialized to the size of your training set). RawThresholds is a good way to build benchmark programs, but for real use cases, PercentageThresholds is the only practical choice.

## Publications

For more information on ConfigV and the theory behind how/why it works see:

- Synthesizing configuration file specifications with association rule learning, OOPSLA18, https://dl.acm.org/citation.cfm?doid=3133888
- Probabilistic Automated Language Learning for Configuration Files, CAV17, https://link.springer.com/chapter/10.1007%2F978-3-319-41540-6_5

## Maintainers

- Mark Santolucito

Feel free to reach out if you have any questions about the tool or how to use it - happy to help!

