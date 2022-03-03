if not exist .\debug\ (
    mkdir .\debug\
)

if exist .\debug\lscomp.exe (
    del .\debug\lscomp.exe
)

ghc --make main.hs -o lscomp.exe 

move lscomp.exe .\debug\

del *.o
del *.hi