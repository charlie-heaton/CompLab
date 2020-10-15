#!/bin/bash
chmod u+x *
dos2unix Shell/*.sh
module load gcc
module load openmpi
module load castep
module switch castep/20.1 castep/18.1
module list
