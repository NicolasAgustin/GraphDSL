int veces = int(input("Cuantas veces queres tu nombre: "));

string contenido = "";

for (0 to veces){
    #write("ejemplos/archivo.txt", "nico" & str(i) & "\r\n", true);
    print("Se imprimio " & str(veces))
};

contenido = read("ejemplos/archivo.txt");

print("El contenido final del archivo es: " & contenido)