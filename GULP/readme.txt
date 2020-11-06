README
---------------------------------------------------------------------------------
DEPENDENCIES:

GULP 5.2
http://gulp.curtin.edu.au/gulp/
Gnuplot
http://www.gnuplot.info/
---------------------------------------------------------------------------------
RUNNING THE PROGRAM:

When running this program, the filepath of GULP will be necessary to
input when prompted.

To use this program, move to the directory /GULPfinal in terminal and
run the bash script executable.sh using ./executable.sh. The main output
should be a graph called 'graphenergies.png'

This script will modify the base input file MgO.gin included in this repository
to various ratios of MgO to CaO, saving them as different input files.
These are run using GULP, with the outputs saved under MgO_xCa.gout, where x depends
on the ratio of of Mg to Ca atoms in the cell. The outputs are read using readEnergiesv1.2
and a graph of the lattice energies created using Gnuplot.
---------------------------------------------------------------------------------
This program is created with the idea that the base input file MgO can be
edited manually e.g changing the potential used for calculation, calculating free
energy instead and the effects of this change can be investigated across varying
ratios of Mg and Ca. These effects can be then compared to other experimental
methods of investigating this crystal structure to test their validity.
---------------------------------------------------------------------------------
FAQS:

Q: 'I'm getting the error:
error while loading shared libraries: libgfortran.so.5: cannot open shared object file: No such file or directory GULP
What can I do?'

A: This error can be fixed by a clean install of GULP.


Q: 'Entering the filepath of the gulp file each time is annoying. Can I automate this?'

A: Yes. By opening executable.sh with a text editor and replacing each instance of '$filepath'
with your filepath and removing the line that asks the user for their filepath, the program
will automatically use the path you have specified.


Q: 'What are some other keywords I can try adding to the base input file MgO.gin?'

A: free_energy,
Note for free_energy: Not correctly functioning for one of the ratios of Mg to Ca
When adding keywords to the input file it is vital that you add these keywords to the first
line of only the MgO.gin input file (ie not MgO_0Ca.gin, MgO_1Ca.gin etc.). This is because it
is used as the basis for each input file created.
