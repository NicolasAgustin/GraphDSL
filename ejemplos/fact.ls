int evaluar = 4;
int n = evaluar;
int factorial = 1;
if (n < 2) {
    pass
}else{
    while(n != 1){
        factorial = factorial * n;
        n = n - 1
    }
    #for(int i = 0;n!=1;i=i+1){
    #    factorial = factorial * n;
    #    n = n - 1
    #}
};

print("El factorial de " & str(evaluar) & " es: " & str(factorial))
