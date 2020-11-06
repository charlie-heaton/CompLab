#!/bin/bash
DDPT=$PWD/NEW_DDPT
BIN=$DDPT/bin

$BIN/GENLATTICE -a1 4.380 -a2 4.760 -type1 Mg -type2 Ca 

grep -v "O" input.pdb > targetswaps.pdb

$BIN/CREATESWAP
rm input.pdb targetswaps.pdb

