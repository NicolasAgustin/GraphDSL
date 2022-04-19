int x = 0;
int y = 0;
int max = 2;
int id = 0;
string node1 = "nodo1";

GRAPH("nuevo", 25)

    for (0 to max){
        for (0 to max){
            node {"nodo" & str(id)} <x+1,y+1>;
            log(str(x) & " " & str(y));
            y = y + 1;
            id = id + 1
        };
        y = 0;
        x = x + 1
    }

END

