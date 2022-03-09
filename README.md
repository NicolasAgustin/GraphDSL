# GraphDSL
DSL para crear diagramas de grafos en latex

## Archivo fuente

El archivo fuente debe tener la extension `.ls`

## Ejecucion con GHCI
Estando en el directorio raiz del proyecto ejecutamos el comando \
`ghci Main.hs` \
Dentro del prompt del interprete ejecutamos\
`Main*> run "(path al archivo)" [True | False]`
# Compilacion GHC
## Para Windows
Ejecutar el script `make.bat`
> .\make.bat
## Para Linux 
Ejecutar el script `make.sh`
> ./make.sh
# Ejecucion
El ejecutable final se encuentra en la carpeta `debug`.
> graphdsl (filepath)
