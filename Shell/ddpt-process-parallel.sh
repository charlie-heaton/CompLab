#!/bin/bash
#Usage ./ddpt-process-parallel.sh 2
#Runs several DDPT programs on a input file generated from the GENLATTICE program. Edited version of ddpt-process.sh to be run on physlog
#Run swapfile.sh before to create the swappairs.txt file

#Path Variables
START=$PWD
DDPT=$START/NEW_DDPT
BIN=$DDPT/bin
PLOT=$DDPT/plot

mkdir -p WORKING_$1
cp $BIN/GENENMM $BIN/GENLATTICE $BIN/FREQEN $BIN/DIAGSTD $BIN/SPACING $BIN/CROSCOR $PLOT/plot_dist_cross.py swappairs.txt WORKING_$1
cd WORKING_$1


echo Running DDPT for Ca Mg Oxide Mixing Project >> log_$1
echo -e '\t'`hostname` at `date`>> log_$1

echo Extracting $1 lines from the big swap file

START=1
awk -v start="$START" -v end="$1" 'NR>=start&&NR<=end' swappairs.txt > swapfile

#Generate the input file 
echo STARTING G E N L A T T I C E >>log
\time --output=log --append -f "Program: %C\nTotal time: %E" ./GENLATTICE -a1 4.380 -a2 4.760 -type1 Mg -type2 Ca -swaps swapfile

#Generate the network model using GENENMM (Edited for an anisotropic network model)
echo STARTING G E N E N M M >> log
\time --output=log --append -f "Program: %C\nTotal time: %E" ./GENENMM -i input.pdb -mass -c 6
#Diagonalise the hessian matrix
echo STARTING D I A G S T D >> log
\time --output=log --append -f "Program: %C\nTotal time: %E" ./DIAGSTD -i matrix.sdijf 

#Calculate mode energies and frequencies from the eigenvalues and eigenvectors for the first 30 non trivial modes.
echo STARTING F R E Q E N >> log
\time --output=log --append -f "Program: %C\nTotal time: %E" ./FREQEN -s 7 -e 36 -i matrix.eigenfacs

#Run analysis from the generated data. Start by calculating crosscorrelations between pairs of atoms.
echo STARTING C R O S C O R >> log
\time --output=log --append -f "Program: %C\nTotal time: %E" ./CROSCOR -s 7 -e 36 -i matrix.eigenfacs

#Create the dist.dat (distance between pairs of atoms) file
echo STARTING S P A C I N G >> log
\time --output=log --append -f "Program: %C\nTotal time: %E" ./SPACING -i input.pdb

#Plot the distance cross correlation graph
echo STARTING P L O T  D I S T /  C R O S S >> log
\time --output=log --append -f "Program: %C\nTotal time: %E" python plot_dist_cross.py

mv Dist_Cross.png Dist_Cross_$1

#Calculate the mode energy for a mode summation of 30 modes.
awk 'NR>1 {print $4}' mode.energy >> energy_30

energy_sum=$(gawk '{ sum += $1}; END {print sum}' energy_30)

echo The free energy of the configuration is $energy_sum >> log

printf " %4s %8.3f\n" $1 $energy_sum >> energies.final_$1
echo ALL DONE >> log

mv log log_$1
mv dist.dat dist_$1 
mv crosscor.dat crosscor_$1
mv dist_$1 crosscor_$1 energies.final_$1 Dist_Cross_$1 log_$1 ..
cd ..
rm -r WORKING_$1
