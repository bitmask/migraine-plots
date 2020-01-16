import sys
import datetime

datafile = sys.argv[1]
if datafile:
    with open(datafile) as f:
        entries = []
        drugs = set()
        for l in f:
            date, mg, drug = l.rstrip("\n").split("\t")
            entries.append((date, mg, drug))
            drugs.add(drug)

        for thisdrug in drugs:
            start = None
            end = None
            for entry in entries:
                (date, mg, drug) = entry
                if thisdrug == drug:
                    if start is not None:
                        end = date
                        end_dt = datetime.datetime.strptime(end, "%Y-%m-%d %H:%M:%S")
                        delta = end_dt - start_dt
                        for h in range(delta.days):
                            h = h+1
                            print str(start_dt + datetime.timedelta(days=h)) + "\t" + str(savemg) + "\t" + thisdrug
                    start = date
                    start_dt = datetime.datetime.strptime(start, "%Y-%m-%d %H:%M:%S")
                    savemg = mg
                    print str(date) + "\t" + mg + "\t" + drug

