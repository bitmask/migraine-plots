import sys
import datetime
from collections import defaultdict

datafile = sys.argv[1]
if datafile:
    with open(datafile) as f:
        entries = defaultdict(list)
        for l in f:
            # get all times for each drug
            date, mg, drug = l.rstrip("\n").split("\t")
            entries[drug].append((date, mg, drug))

        for thisdrug, entries in entries.items():
            start = None
            end = None
            if len(entries) == 1:
                # if there is only one timestamp on the list, use current date as the end
                for entry in entries:
                    (date, mg, drug) = entry
                    end_dt = datetime.datetime.now()
                    start_dt = datetime.datetime.strptime(date, "%Y-%m-%d %H:%M:%S")
                    delta = end_dt - start_dt
                    for h in range(delta.days):
                        h = h+1
                        print str(start_dt + datetime.timedelta(days=h)) + "\t" + str(mg) + "\t" + drug
            else:
                # go through all the timestamps
                firstzero = 1 # only print the first zero, if drug is stopped and resumed, don't print all the intervening zeros
                for entry in entries:
                    (date, mg, drug) = entry
                    if start is not None:
                        end = date
                        end_dt = datetime.datetime.strptime(end, "%Y-%m-%d %H:%M:%S")
                        delta = end_dt - start_dt
                        for h in range(delta.days):
                            h = h+1
                            if firstzero:
                                print str(start_dt + datetime.timedelta(days=h)) + "\t" + str(savemg) + "\t" + thisdrug
                            else:
                                if savemg > 0: # drug has been resumed, so reset and print the next zero
                                    firstzero=1
                            if savemg == "0":
                                firstzero=0 # don't print any more zeros
                    start = date
                    start_dt = datetime.datetime.strptime(start, "%Y-%m-%d %H:%M:%S")
                    savemg = mg
                    print str(date) + "\t" + mg + "\t" + drug

