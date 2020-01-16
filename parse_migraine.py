import sys
import datetime

datafile = sys.argv[1]
if datafile:
    with open(datafile) as f:
        start = None
        end = None
        saveintensity = 0
        for l in f:
            date, intensity = l.rstrip("\n").split("\t")
            if start is not None:
                end = date
                end_dt = datetime.datetime.strptime(end, "%Y-%m-%d %H:%M:%S")
                start_dt = datetime.datetime.strptime(start, "%Y-%m-%d %H:%M:%S")
                delta = end_dt - start_dt
                seconds = delta.days * 24*60*60 + delta.seconds
                for h in range(seconds):
                    h = h+1 # don't print this entry twice because range starts at 0
                    if h % 3600 == 0:
                        if int(saveintensity) > 0:
                            print str(start_dt + datetime.timedelta(seconds = h)) + "\t" + str(saveintensity)
                
            saveintensity = intensity
            if int(saveintensity) > 0:
                print str(date) + "\t" + str(saveintensity)
            start = date
