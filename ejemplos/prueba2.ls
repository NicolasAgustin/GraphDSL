int n = 2;
int counter = 0;
int max = 10;
string id_anterior = "";
string id_actual = "";
GRAPH("test", 25)

    insert{
        "root",
        "root"
    };

    for(0 to max){
        
        id_actual = "node" & str(counter);
        
        log(id_actual);
        log(id_anterior);
        log(str(counter));

        if(counter == 0){
            insert{
                id_actual,
                id_actual,
                below right of root
            }
        } else {
            id_anterior = "node" & str(counter - 1);
            insert{
                id_actual,
                id_actual,
                below of "id_anterior"
            }
        };

        edge {root} -> {"id_actual"};
        
        counter = counter + 1
    }

END