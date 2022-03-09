#! /bin/bash

if [ ! -d ./debug ]; then 
    mkdir debug
fi

if [ -f ./debug/graphdsl ]; then
    rm ./debug/graphdsl
fi

ghc --make Main.hs -o graphdsl

rm *.o *.hi

mv graphdsl ./debug

