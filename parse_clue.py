import sys
import json
import pprint

datafile = sys.argv[1]
if datafile:
    with open(datafile) as f:
        data = json.load(f)

    
for entry in data['data']:
    if 'period' in entry:
        print entry['day'] + "\t" + entry['period']
    #if 'pain' in entry:
        #print entry['day'] + "\t" + entry['pain'][0]
