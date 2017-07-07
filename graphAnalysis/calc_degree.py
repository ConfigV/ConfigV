#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# ===== calc_degree.py =====
# calculates and outputs degree metric for each vertex in the rule graph
#
# USAGE:
#
#     calc_degree.py [EDGE_FILE] [DEGREE_FILE]
#

import argparse
import json
import operator
import pprint
import sys

parser = argparse.ArgumentParser(description=\
        'calculate degree for vertices in edge file')
parser.add_argument('edge_file')
parser.add_argument('degree_file')
args = parser.parse_args()

with open(args.edge_file) as data_file:
    edges = json.load(data_file)

vertices = set()

for edge in edges:
    vertices = vertices \
               | set(edge['PARTICIPANT_A']) \
               | set(edge['PARTICIPANT_B'])

degrees = {}

for v in vertices:
    for edge in edges:
        if (v in edge['PARTICIPANT_A'] or
            v in edge['PARTICIPANT_B']):

            if v in degrees:
                degrees[v] = degrees[v] + edge['WEIGHT']
            else:
                degrees[v] = edge['WEIGHT']

# prune nodes without context and add their degrees
# to context-bearing nodes
'''
to_delete = []

for d1 in degrees:
    if "[" not in d1:
        for d2 in degrees:
            if (d2.startswith(d1)
                    and not d1 == d2):
                degrees[d2] = degrees[d2] + degrees[d1]
        to_delete.append(d1)

for d in to_delete:
    degrees.pop(d, None)
'''
# done pruning

sorted_degrees = sorted(degrees.items(), key=operator.itemgetter(1))

with open(args.degree_file, 'w') as outfile:
    json.dump(sorted_degrees, outfile)
