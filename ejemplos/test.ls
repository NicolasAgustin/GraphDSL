int i = 0;
int max = 10;
int a = 1;
int flag = 0;
string nombre_nodo = "";
string anterior_id = "";
string id = "";

GRAPH("grafico_ciudad", 5)
    while(i < max){
        string id = str(i);
        if (flag != 0) {
            anterior_id = str(int(id) - 1)
        } else {
            flag = 1;
            anterior_id = id;
            insert(id, id)
        };
        nombre_nodo = id;
        insert(
            nombre_nodo,
            id,
            below left of anterior_id
        );
        i = i + 1
    }
END;

insert(
    "1",
    "hola"
);

log("Probando probando probando mi amor por ti")