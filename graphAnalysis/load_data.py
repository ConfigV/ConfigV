#!/usr/bin/env python

import pymongo as pym
from bson import son

mc = pym.MongoClient("localhost:27018")
db = mc['cfg']

nodesColl = db['nodes']
edgesColl = db['edges']

import json

with open('cachedRules.json') as data_file:
    data = json.load(data_file)

# add all rule types as targets
RULE_TYPES = [
    'orderl'
    ,'missingl'
    ,'intRell'
    ,'typeErrl'
]

for t in RULE_TYPES:
    for edge in data[t]:

        for edgeType in edge[1]:

            if isinstance(edge[0], basestring):
                # self edge
                edgesColl.update(
                    son.SON([
                        (u'_id', edge[0] + edge[0] + t + edgeType)
                    ])
                    ,son.SON([
                        (u'PARTICIPANT_A', edge[0])
                        ,(u'PARTICIPANT_B', edge[0])
                        ,(u'EDGE_GROUP', t)
                        ,(u'EDGE_TYPE', edgeType)
                        ,(u'EDGE_VALUE', edge[1][edgeType])
                    ]),
                    upsert=True, multi=False)

            else: #self edge

                edgesColl.update(
                    son.SON([
                        (u'_id', edge[0][0] + edge[0][1] + t + edgeType)
                    ])
                    ,son.SON([
                        (u'PARTICIPANT_A', edge[0][0])
                        ,(u'PARTICIPANT_B', edge[0][1])
                        ,(u'EDGE_GROUP', t)
                        ,(u'EDGE_TYPE', edgeType)
                        ,(u'EDGE_VALUE', edge[1][edgeType])
                    ]),
                    upsert=True, multi=False)

        # construct the edge

    

