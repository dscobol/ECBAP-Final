#!/bin/bash

# Clean up
rm ../data/addrout.dat.txt
rm ../data/partout.dat.txt
rm ../data/poout.dat.txt
rm ../data/psapout.dat.txt
rm ../data/suppout.dat.txt

rm POCALL.dll
rm ADDRCALL.dll
rm SUPPCALL.dll
rm PARTCALL.dll
rm PSAP.exe

cobc -m POCALL.cbl
cobc -m ADDRCALL.cbl
cobc -m SUPPCALL.cbl
cobc -m PARTCALL.cbl
cobc -x -o PSAP PSAP.cbl

if [ "$?" -eq 0 ]; then
    ./PSAP.exe
else
    echo "Complier Return code not ZERO."
fi