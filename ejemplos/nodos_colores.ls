string nombre = "";
int max = 2;
int counter = 0;
int fila = 0;
int columna = 0;
GRAPH("", 5)[2]
    while (fila < max) {
        while (columna < max){
            nombre = "n" & str(counter);
            node {nombre}<fila,columna>;
            counter = counter + 1;
            columna = columna + 1
        };
        columna = 0;
        fila = fila + 1
    }
END 