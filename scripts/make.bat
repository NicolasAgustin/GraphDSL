if not exist .\debug\ (
    mkdir .\debug\
)

if exist .\debug\graphdsl.exe (
    del .\debug\graphdsl.exe
)

ghc --make main.hs -o graphdsl.exe 

move graphdsl.exe .\debug\

del *.o
del *.hi