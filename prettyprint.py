import json 
import commands

with open("cachedRules.json", "r") as read_file:
    rules = json.load(read_file)

kvk = rules['keyvalkeyl']

for r in kvk:
  cmd = "git grep " + (r[0]['k1']['uninternedText']) + " Datasets/antonDumpCSV/* | wc -l"
  count = (commands.getstatusoutput(cmd)[1])
  if (str(count) != str(r[1]['antiRuleData']['tru'])):
    print count
    print r[0]['k1']['uninternedText'] + " = " + r[0]['v1']['uninternedText']
    print "     => " + r[0]['k2']['uninternedText']
    print r[1] 


