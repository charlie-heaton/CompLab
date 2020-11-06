#!/bin/bash

for index in $(seq 1 1 128)
do
	cat energies.final_$index >> resultfile
	mv Dist_Cross_$index Dist_Cross_$index.png
done
mkdir logs 
mkdir Crosscor
mkdir Dist
mv crosscor_* Crosscor
mv dist_* Dist
mv log_* logs
mv Dist_Cross_* Dist_Cross_*.png
rm energies.final_*
