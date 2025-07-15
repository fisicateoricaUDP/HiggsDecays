#!/bin/bash

var1=147 # Number of 1PI diagrams
var2=95 # Number of non-1PI diagrams

name=MSSM

MAX_JOBS=2 # Maximum number of parallel processes 
JOBS=0

# Generation of 1PI diagrams and amplitudes

for i in $(seq 1 $var1); do
    nohup wolframscript -script 1_wzgAmp.wls $i $name Reducible opi >& "log1pi_${i}" &!

    ((JOBS++))

    if ((JOBS >= MAX_JOBS)); then
        wait -n  
        ((JOBS--))  
    fi
done

wait
echo "All the 1PI amplitudes has been generated!"

# Generation of non-1PI diagrams and amplitudes

for i in $(seq 1 $var2); do
    nohup wolframscript -script 1_wzgAmp.wls $i $name Irreducible nopi >& "logn1pi_${i}" &!

    ((JOBS++))

    if ((JOBS >= MAX_JOBS)); then
        wait -n  
        ((JOBS--))  
    fi
done

wait
echo "All the non-1PI amplitudes has been generated!"

# Sum of divergent and finite parts for 1PI and non-1PI diagrams

nohup wolframscript -script 2_divfin.wls $var1 $var2 $name >& logdf &!

wait
echo "The finite and divergent parts in MSSM were saved!"
