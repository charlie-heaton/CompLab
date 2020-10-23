README

This program requires GULP already be downloaded and gnuplot installed.

When running this program, the filepath of GULP will be necessary to 
input when prompted.

To use this program, move to the directory /GULPfinal in terminal and
run the bash script executable.sh using ./executable.sh. The main output
should be a graph called 'graphenergies.png'

This script will modify the base input file MgO.gin included in this repository
to various ratios of MgO to CaO, saving them as different input files.
These are run using GULP. The outputs are read using readEnergiesv1.1 and
a graph of the lattice energies created using gnuplot.

This program is created with the idea that the base input file MgO can be
edited manually e.g changing the potential used for calculation, and the
effects of this change can be investigated across varying ratios of Mg and Ca.
