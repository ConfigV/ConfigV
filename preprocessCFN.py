import json
import yaml
import csv
import os
from collections import Counter

class CFNData:
    def __init__(self, key, value, resourceType, name):
        self.key = key
        self.value = value
        self.resourceType = resourceType
        self.name = name

# process json CFN templates into the intermediate csv form for ConfigV

################
##            ##
##  CONSTANTS ##
##            ##
################

targetDir = "templates4"
resultsDir = "templates4_2CSV"
fileLimit = 99999

############
##        ##
##  CODE  ##
##        ##
############
def flattenjson( node, parent, delim ):
    val = {}
    for i in node.keys():
        # we are not at a leaf
        if isinstance( node[i], dict ):
            child = flattenjson( node[i], i, delim )
            for j in child.keys():
                #skip the name
                if parent=="Resources":
                    if "Type" in child.keys():
                        val[ child["Type"] + "." + j ] = child[j]
                    else:
                        val[ j ] = child[j]
                elif (j=="Ref" or ("Fn::" in j)): #treat as leaf node
                    val[i] = node[i]
                else:
                    val[ i + delim + j ] = child[j]
        # if we are at a leaf
        else:
            val[i] = node[i]
    return val

def jsonToCSV(inputJsonFilePath):
    with open(inputJsonFilePath) as f:
        jsonData = json.load(f)
    if not isinstance( jsonData, dict):
        flattenedJson = {}
    else:
        flattenedJson = flattenjson(jsonData, "", "." )
    isRelevent = lambda k: len([c for c in k if c=="."])>=2 and not ("east" in k or "west" in k or "north" in k or "south" in k or "InstanceType" in k)
    return [(k,v) for (k,v) in flattenedJson.items() if isRelevent(k)]

flatten = lambda l: [item for sublist in l for item in sublist]

def convertDataset(pathToDset):
   
    fileCounter = 0 
    for root, subdirs, files in os.walk(pathToDset):
        for filename in files:
            if fileCounter >= fileLimit:
                break
            print(filename)
            os.system("cfn-lint "+os.path.join(root,filename))

            try:
                templateData = jsonToCSV(os.path.join(root, filename))
                with open( resultsDir+"/"+filename+".csv", 'wb' ) as out_file:
                    csv_w = csv.writer( out_file )
                    for keyVal in templateData:
                        csv_w.writerow(keyVal)
                fileCounter += 1
            except ValueError, e:
                print("failed to process: "+filename)
                print(e)
                continue

if not os.path.exists(resultsDir):
    os.mkdir(resultsDir)

try:
  convertDataset(targetDir)
except KeyboardInterrupt:
  sys.exit()
 
