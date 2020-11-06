#!/bin/bash
echo -e "Please enter the filepath of your gulp file: (e.g xx/yy/zz/gulp-5.2/Src/gulp)"
read filepath
./MgtoCa.sh
$filepath ./MgO_0Ca
$filepath ./MgO_1Ca
$filepath ./MgO_2Ca
$filepath ./MgO_3Ca
$filepath ./MgO_4Ca
./readEnergiesv1.22
