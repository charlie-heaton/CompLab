NEW-DDPT is a software solution to lattice dynamics and metal oxide mutual solubility based on the DDPT software (Durham Dynamic Protein Toolbox) 
by using a Born-Mayer potential instead of a basic harmonic potential. It is an edited version of DDPT that allows for a simplified input file (coordinates and atom type).
To run, an inputfile needs to be generated. This can be done by GENLATTICE. GENENMM produces a mass weighted Hessian matrix that is diagonalised by DIAGSTD to find 
eigenvalues and eigenvectors of the Hessian matrix. FREQEN takes the eigenvalues and calculates free energies for the configuration, CROSCOR calcualtes cross correlations 
for atomic pairs, SPACING calculates distances between atomic paris and plot_dist_cross.py plots a distance/cross correlation diagram for atomic pairs.

USAGE:
From the home directory of this downloaded repository

cd NEW_DDPT
./configure (you may need to run dos2unix configure and/or chmod u+x configure to run)
make
make install (if you want to change path variables and install the software so it can run from any directory)
Enjoy!

To carry out simulations that contributed to this experiment run the following three shell scripts in the home directory of this repository
./Shell/swapfile.sh
./Shell/submit.job
./Shell/results.sh
Where submit.job is an array job submission script that was used to run ./Shell/ddpt-process-parallel.sh $index over 128 different indicies to simulate the substitution of 
atoms.
To see an example of how DDPT can be used. See ddpt-process-parrallel for an example.
