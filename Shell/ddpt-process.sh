#!/bin/bash

#Runs several DDPT programs on a input file generated from the GENLATTICE program.

#Path Variables
DDPT  =  NEW_DDPT
BIN   =  $NEW_DDPT/bin
PLOT  =  $NEW_DDPT/plot

echo Running DDPT for Ca Mg Oxide Mixing Project at $date >> log
#Generate the input file
\time --output=log --append -f "Program: %C\nTotal time: %E" $BIN/GENLATTICE -aMg 4.380 -aCa 4.760
#Generate the network model using GENENMM (Edited for an anisotropic network model)
\time --output=log --append -f "Program: %C\nTotal time: %E" $BIN/GENENMM -i input.pdb -mass -c 6
#Diagonalise the hessian matrix
\time --output=log --append -f "Program: %C\nTotal time: %E" $BIN/DIAGSTD -i matrix.sdijf 
#Calculate mode energies and frequencies from the eigenvalues and eigenvectors
\time --output=log --append -f "Program: %C\nTotal time: %E" $BIN/FREQEN -s 7 -e 36 -i matrix.eigenfacs

#Run analysis from the generated data. Start by calculating crosscorrelations between pairs of atoms.
\time --output=log --append -f "Program: %C\nTotal time: %E" $BIN/CROSCOR -s 7 -e 36 -i matrix.eigenfacs
#Create the dist.dat (distance between pairs of atoms) file
\time --output=log --append -f "Program: %C\nTotal time: %E" $BIN/SPACING -i input.pdb
#Plot the distance cross correlation graph
\time --output=log --append -f "Program: %C\nTotal time: %E" python $PLOT/plot_dist_cross.py 
#Calculate the mode energy for a mode summation of 30 modes.
awk 'NR>1 {print $4}' mode.energy >> energy_30
Free_Energy = awk '{sum+=$1} END {print sum}' energy_30
echo The free energy of the configuration is $Energy >> log
echo The free energy of the configuration is $Energy