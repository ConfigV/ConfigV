#!/usr/bin/env python
#
# ==============================
# ===== calc_complexity.py
# ==============================
# Calculate the complexity of a configuration file using data extracted
# from an intermediate representation containing learned rules between
# configuration file options
#
# distances are calculated
#
# REQUIRES:
#
#   rule graph edge definitions from `build_graph.py`
#
# USAGE:
#
#   calc_complexity.py <config_opts> <edge_file>
#
# where config file options should be provided in a file, each parameter
# a string, delimited by '\n' characters.

import argparse
import json

parser = argparse.ArgumentParser(description=\
        'analyze complexity of configuration file')
parser.add_argument('config_opts')
parser.add_argument('edge_file')
args = parser.parse_args()

#
# determine the "total weight" between an option
# and a set of options
#
def weight_between(opt, opts, edges):

    weight = 0
    for edge in edges:
        if (opt in edge['PARTICIPANT_A']
            and set(edge['PARTICIPANT_B']) & set(opts)):
            weight = weight + edge['WEIGHT']

    return weight

def total_weight(opt, edges):

    weight = 0
    for edge in edges:
        if (opt in edge['PARTICIPANT_A']):
            weight = weight + edge['WEIGHT']

    return weight


# CONFIG FILE COMPLEXITY(opts):
#
#       candidate_complexity = 0
#
#       for opt_a in opts:
#           opt_weight = 0;
#
#           all_local_weight = total_weight(opt_a)
#
#           for opt_b in opts: # since self-edges allowed
#               opt_weight = opt_weight + weight_between(opt_a, [opt_b])
#
#
#           # now how related are they?
#           # if A is heavily related to other options, count it less.
#
#           candidate_complexity =
#                       candidate_complexity +
#                       1 * (1 - (opt_weight / all_local_weight))
#

with open(args.config_opts) as opts_file:
    config_options = opts_file.readlines()

config_options = [x.strip() for x in config_options]

with open(args.edge_file) as edge_file:
    edges = json.load(edge_file)

candidate_complexity = float(0)

for opt_a in config_options:
    opt_weight = 0

    all_local_weight = total_weight(opt_a, edges)

    for opt_b in config_options:
        opt_weight = opt_weight + weight_between(opt_a, [opt_b], edges)

    if all_local_weight != 0:
        candidate_complexity = \
                candidate_complexity + \
                1 * (1 - (opt_weight / all_local_weight))
    else:
        # no information available, use naive measure
        candidate_complexity = candidate_complexity + 1

print ("INFO: config file processing completed")
print ("INFO: {} lines, {} total complexity score".format(
                     len(config_options), candidate_complexity))
