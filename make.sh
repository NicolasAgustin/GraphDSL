#! /bin/bash

if [ ! -d ./debug ]; then 
    mkdir debug
fi

if [ -f ./debug/lscomp ]; then
    rm ./debug/lscomp
fi

ghc --make main.hs -o lscomp

rm *.o *.hi

mv lscomp ./debug

