#!/usr/bin/env bash
#

if [ "$1" == "" ]; then
    echo "rerunning with existing log"
else
    if [ -f $1 ]; then
        cat $1 | awk -F"," '{print $2 "\t" $3}' | sed -e '1,9d' | sed 's/ //' | sed $'s/\t/:00\t/' | sed 's/Eletriptan/40mg eletriptan/' >> data/log
        echo "edit log because not everything can be automated"
        echo " - comment out any comments that should not be parsed"
        echo " - add preventative to changes in daily meds"
        echo " - make sure last line is not blank"
        echo ""
        echo "press enter when done"
        read -n 1
    else
        echo "file $1 does not exist"
        exit 1
    fi
fi


if [ ! -d data/parsed ]; then
    mkdir -p data/parsed
fi
if [ ! -d plots ]; then
    mkdir plots
fi

python parse_clue.py $(find . -type f -name "ClueBackup-*" |sort|tail -n 1) |sort|uniq|grep -E 'light|medium|heavy|spotting' > data/parsed/clue.read
cat data/log | grep -v "#" | grep -i aura > data/parsed/aura
cat data/log | grep -v "#" | grep -i naus > data/parsed/nausea
grep -v "#" data/log|grep triptan|sed 's/mg /	/' > data/parsed/drugs
grep preventative data/log|sed 's/preventative.*$//'|sed 's/mg /	/' | sed 's/ $//' > data/preventatives
python parse_preventatives.py data/preventatives > data/parsed/preventatives_start_end

cat data/log | grep -v "#" | grep -vi aura | grep -vi naus | grep -v mg | grep -v cannabis |grep -v amitriptiline |grep -v preventative|sort > data/migraine_current
cat data/migraine_historical data/migraine_current > data/migraine
python parse_migraine.py data/migraine > data/parsed/migraine_start_end

./COMMANDS.R

