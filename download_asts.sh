#!/usr/bin/env bash

set -eo pipefail
set -x

FILEID=19BPHcVqO7S-tRCvljrBszyGzD1T2Fmxo
FILENAME=nested_asts.tar.gz

str2="https://drive.google.com/uc?export=download&id=$FILEID"
str3="s/.*confirm=([0-9A-Za-z_]+).*/\1\n/p"
str4="https://drive.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate $str2 -O- | sed -rn $str3)&id=$FILEID"

rm -rf nested_asts.tar*

wget $str4 --load-cookies /tmp/cookies.txt  -O $FILENAME && rm -rf /tmp/cookies.txt

tar -xvf $FILENAME
mkdir -p data/nested_asts/
mv nested_asts/* data/nested_asts/
rmdir nested_asts
