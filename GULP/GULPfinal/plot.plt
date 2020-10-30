set term postscript eps enhanced color font "Helvetica" 18
set nokey
set grid
set size 0.8, 0.8
set size ratio 0.7
set xlabel "Number of Ca atoms"
set xtic(0,1,2,3,4)
set mytics 5
set ylabel "Lattice Energy (eV)"
set output 'graphenergies.eps'      
m="energies.dat"
p m u 1:2 w lp
