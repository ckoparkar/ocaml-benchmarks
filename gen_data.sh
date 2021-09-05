#!/usr/bin/env bash

set -eo pipefail
set -x

mkdir -p data
cd pbbs/nearestNeighbors/geometryData
make

./plummer.exe -d 3 12800 ../../../data/plummer_3d_12800.txt
./plummer.exe -d 3 1000000 ../../../data/plummer_3d_1000000.txt
./plummer.exe -d 3 2000000 ../../../data/plummer_3d_2000000.txt

./uniform.exe -d 2 12800 ../../../data/uniform_2d_12800.txt
./uniform.exe -d 2 1000000 ../../../data/uniform_2d_1000000.txt
./uniform.exe -d 2 2000000 ../../../data/uniform_2d_2000000.txt

cd ../../../data/

sed '1d' plummer_3d_12800.txt > tmpfile; mv tmpfile plummer_3d_12800.txt
sed '1d' plummer_3d_1000000.txt > tmpfile; mv tmpfile plummer_3d_1000000.txt
sed '1d' plummer_3d_2000000.txt > tmpfile; mv tmpfile plummer_3d_2000000.txt

sed '1d' uniform_2d_12800.txt > tmpfile; mv tmpfile uniform_2d_12800.txt
sed '1d' uniform_2d_1000000.txt > tmpfile; mv tmpfile uniform_2d_1000000.txt
sed '1d' uniform_2d_2000000.txt > tmpfile; mv tmpfile uniform_2d_2000000.txt
