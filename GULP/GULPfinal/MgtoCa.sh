#!/bin/sh

#Bash script runs MgOtoCa for all possible amount of Ca atoms

for i in  0 1 2 3 4
do
#Inputs variable to MgOtoCa.f90, and puts output in new .dat file
    rm -f MgtoCa.in
    echo $i> MgOtoCa.in
    rm MgOtoCa.dat
    ./MgtoCa < MgOtoCa.in 
    mv MgOtoCa.dat MgO_${i}Ca.gin

done
