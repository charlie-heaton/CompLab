set term png
set title "Lattice energy of MgO and CaO mixture"
set nokey
set grid
set xlabel "Ratio of Ca to Mg"
set xtic(0,0.25,0.5,0.75,1)
set ylabel "Lattice Energy (eV)"
set output 'graphenergies.png'
m="energies.dat"
plot m using 1:2 with linespoints
