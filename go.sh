#!/usr/bin/env bash
#

if [ "$1" == "" ]; then
    echo "press enter to rerun with existing log and not add new data"
    echo "otherwise, pass new export as parameter"
    read -n 1
else
    if [ -f $1 ]; then
        cat $1 | sed -e '1,5d' | perl -nale '@col=split /,/; $text = $col[2]; $text=~s/"//g; @dt=split " ", $col[1]; $date=$dt[0]; $time=$dt[1] . ":00"; @dmy=split "/", $date; print $dmy[2] . "-". $dmy[1] ."-". $dmy[0] ." ". $time ."\t". $text' >> data/log
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
grep -v "#" data/log|grep mg|grep -v preventative|sed 's/mg /	/' > data/parsed/drugs
grep preventative data/log|sed 's/preventative.*$//'|sed 's/mg /	/' | sed 's/ $//' > data/preventatives
python parse_preventatives.py data/preventatives > data/parsed/preventatives_start_end

cat data/log | grep -v "#" | grep -vi aura | grep -vi naus | grep -v mg | grep -v cannabis |grep -v amitriptiline |grep -v preventative|sort > data/migraine_current
cat data/migraine_historical data/migraine_current > data/migraine
python parse_migraine.py data/migraine > data/parsed/migraine_start_end

./COMMANDS.R

