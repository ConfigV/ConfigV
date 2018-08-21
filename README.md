# ConfigV

A tool for learning rules about configuration langauges. 

ConfigV uses a generalization of Association Rule Learning to learn user-defined predicates over datasets of configuration files. You can think of this as data science meets programming languages meets systems.

## Basic Testing

Install with 'cabal install' and run the executable with /home/tester/.cabal/bin/ConfigV to see learning and verification in action on a toy example.
To set up ConfigV with the testing configuration, open src/Settings.hs, and set the below settings. This will make the tool learn from the files in the directory named testLearn and verify the files in the directory named “user”. Note that both testLearn and user directories are preloaded with simple configurations to test that the tool is working correctly.

- trainingTarget = Test
- verificationTarget = "user"

To run the tool on a large example, change these settings to:

- trainingTarget = Prob
- verificationTarget = "githubFiles"

## Helpful Tips

The two key files you will need to know about in order to use ConfigV are as follows:

- src/Settings.hs: all the useful settings to configure the tool. This file is documented in detail for each setting. NB, after changing any setting,you must cabal install again.
- cachedRules.json: the rules that were learned in json form. This can be manually inspected as a sanity check. To pretty print this file,you call ./viewRules.sh

Support and Confidence If you want to see the effect of different support and confidence thresholds, simply edit the threshold settings in the Settings.hs file. The values are present to the values used in the evaluation in the paper. Your own input The best way to explore the tool at first is to keep the testing configuration and change values in the ’testLearn’ files or the ’user’ files. After adding more configuration settings (make up any keywords and values you
like) to ’testLearn’, you can inspect cachedRules.json to directly see what was learned. You can also change the support and confidence thresholds in src/Settings.hs to control when rules will be accepted (although on the small testLearn training set this isn’t very interesting).

## Publications

For more information on ConfigV and the theory behind how/why it works see:

- Probabilistic Automated Language Learning for Configuration Files, CAV17, https://link.springer.com/chapter/10.1007%2F978-3-319-41540-6_5
- Synthesizing configuration file specifications with association rule learning, OOPSLA18, https://dl.acm.org/citation.cfm?doid=3133888
