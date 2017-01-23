#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# ===== build_graph.py =====
# builds the Rule Graph from JSON-formatted output from VeriConf
#
# USAGE:
#
#     build_graph.py [INPUT_FILE] [EDGE_FILE] [VERTEX_FILE]
#

import argparse
import json
import operator
import pprint
import sys

parser = argparse.ArgumentParser(description=\
        'Build VeriConf post-analysis rule graph')
parser.add_argument('input_file')
parser.add_argument('edge_file')
args = parser.parse_args()

with open(args.input_file) as data_file:
    data = json.load(data_file)

# DEFINE RULE PARSING LOGIC

def parse_fine_l(x):

    nodes = x[0]

    sources = nodes[0:2]
    targets = [nodes[2]]

    vals = x[1]

    label = max(vals.iteritems(), key=operator.itemgetter(1))[0]

    tru = float(vals[label])
    total = float(sum(vals.values()))

    return {
        'PARTICIPANT_A' : sources
        ,'PARTICIPANT_B' : targets
        ,'LABEL' : label
        ,'WEIGHT' : tru / total
    }

def parse_order_l(x):

    nodes = x[0]

    sources = [nodes[0]]
    targets = [nodes[1]]

    vals = x[1]

    label = 'order'

    tru = float( vals['tru'] )
    total = float( vals['tru'] + vals['fls'] )

    return {
        'PARTICIPANT_A' : sources
        ,'PARTICIPANT_B' : targets
        ,'LABEL' : label
        ,'WEIGHT' : tru / total
    }

def parse_missing_l(x):

    nodes = x[0]

    sources = [nodes[0]]
    targets = [nodes[1]]

    vals = x[1]

    label = 'missing'

    tru = float( vals['tru'] )
    total = float( vals['tru'] + vals['fls'] )

    return {
        'PARTICIPANT_A' : sources
        ,'PARTICIPANT_B' : targets
        ,'LABEL' : label
        ,'WEIGHT' : tru / total
    }

def parse_int_rel_l(x):

    nodes = x[0]

    sources = [nodes[0]]
    targets = [nodes[1]]

    vals = x[1]

    label = max(vals.iteritems(), key=operator.itemgetter(1))[0]

    tru = float(vals[label])
    total = float(sum(vals.values()))

    return {
        'PARTICIPANT_A' : sources
        ,'PARTICIPANT_B' : targets
        ,'LABEL' : label
        ,'WEIGHT' : tru / total
    }

def parse_type_err_l(x):

    node = x[0]

    if not isinstance(node, basestring):
        sys.exit('ERROR: malformed type rule found in graph construction')

    sources = [node]
    targets = [node]

    vals = x[1]

    label = max(vals.iteritems(), key=operator.itemgetter(1))[0]

    tru = float(vals[label])
    total = float(sum(vals.values()))

    if total > 10:
        return {
            'PARTICIPANT_A' : sources
            ,'PARTICIPANT_B' : targets
            ,'LABEL' : label
            ,'WEIGHT' : tru / total
        }
    else:
        return None

# add all rule types as targets
RULE_TYPES = [
    ('finel', parse_fine_l)
    ,('orderl', parse_order_l)
    ,('missingl', parse_missing_l)
    ,('intRell', parse_int_rel_l)
    ,('typeErrl', parse_type_err_l)
]

edges = []

for t, parser in RULE_TYPES:
    for edge in data[t]:
        parsed_edge = parser(edge)
        if parsed_edge:
            edges.append( parsed_edge )

with open(args.edge_file, 'w') as outfile:
    json.dump(edges, outfile)

