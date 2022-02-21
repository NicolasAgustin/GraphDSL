int veces = int(input("Cuantas veces queres tu nombre: "));

string contenido = "";

for (int i = 0;i<veces;i=i+1){
    write("ejemplos/archivo.txt", "nico" & str(i) & "\r\n", true);
    print("Se imprimio " & str(i))
};

contenido = read("ejemplos/archivo.txt");

print("El contenido final del archivo es: " & contenido)