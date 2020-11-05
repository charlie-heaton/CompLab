#!/bin/bash
#Usage ./ddpt-process-parallel.sh 2 big_swap_file
#Runs several DDPT programs on a input file generated from the GENLATTICE program. Edited version of ddpt-process.sh to be run on physlog
#Run the CREATESWAP program first to create the bigswapfile.

#Path Variables
HOME=$PWD
DDPT=$PWD/NEW_DDPT
BIN=$DDPT/bin
PLOT=$DDPT/plot

echo Running DDPT for Ca Mg Oxide Mixing Project >> log
echo -e '\t'`hostname` at `date`>> log

echo Extracting $1 lines from the big swap file

START=1
awk -v start="$START" -v end="$1" 'NR>=start&&NR<=end' $HOME/$2 > swapfile

#Generate the input file 
\time --output=log --append -f "Program: %C\nTotal time: %E" $BIN/GENLATTICE -a1 4.380 -a2 4.760 -type1 Mg -type2 Ca -swaps swapfile

#Generate the network model using GENENMM (Edited for an anisotropic network model)
\time --output=log --append -f "Program: %C\nTotal time: %E" $BIN/GENENMM -i input.pdb -mass -c 6
#Diagonalise the hessian matrix
\time --output=log --append -f "Program: %C\nTotal time: %E" $BIN/DIAGSTD -i matrix.sdijf 

#Calculate mode energies and frequencies from the eigenvalues and eigenvectors for the first 30 non trivial modes.
\time --output=log --append -f "Program: %C\nTotal time: %E" $BIN/FREQEN -s 7 -e 36 -i matrix.eigenfacs

#Run analysis from the generated data. Start by calculating crosscorrelations between pairs of atoms.
\time --output=log --append -f "Program: %C\nTotal time: %E" $BIN/CROSCOR -s 7 -e 36 -i matrix.eigenfacs

#Create the dist.dat (distance between pairs of atoms) file
\time --output=log --append -f "Program: %C\nTotal time: %E" $BIN/SPACING -i input.pdb

#Plot the distance cross correlation graph
\time --output=log --append -f "Program: %C\nTotal time: %E" python $PLOT/plot_dist_cross.py

#Calculate the mode energy for a mode summation of 30 modes.
awk 'NR>1 {print $4}' mode.energy >> energy_30_$1
echo "$1" >> energy_sum
echo $(sed 's/$/+/' energy_30_$1) 0 | bc >> energy_sum 
echo The free energy of the configuration is $(sed 's/$/+/' energy_30) 0 | bc >> log
rm matrix.* mode.* energy_30*
