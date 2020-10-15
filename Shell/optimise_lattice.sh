#!/bin/bash

for x in $(seq 4 0.01 5)
do 
    for y in $(seq 4 0.01 5) 
    do
        for z in $(seq 4 0.01 5)
        do
   		sed '1,5d' MgO.cell >> cell
   		rm MgO.cell 
   		Bin/WRITE -x $a -y $a -z $a
   		cat coordinates.dat cell >> MgO.cell
   		echo Running Castep for lattice constants x = $x , y = $y , z = $z >> log
   		\time --output=log --append -f "Program: %C\nTotal time: %E" mpirun -np 1 castep.mpi MgO  
   		rm *.usp *.bib *.bands *.castep_bin *.check *.cst_esp
   		grep 'Pressure' MgO.castep >> Pres
   		P=$(awk '{print $3}' Pres)
   		echo Pressure is $P >> log
   		echo >> log
   		printf " %8.3f %8.3f %8.3f %8.3f\n" $x $y $z $P >> pressures_cube.dat
   		rm Pres cell coordinates.dat MgO.castep
 
        done
    done
done

