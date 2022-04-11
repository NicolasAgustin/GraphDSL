GRAPH ("prueba", 20);
int i = 0;
int anterior = 0;
while (i < 5) {
    if (i == 0) {
        insert ("n" & str(i), "x_" & str(i))
    } else {
        anterior = i - 1;
        insert (
            "n" & str(i),
            "x_" & str(i),
            below left of "n" & str(anterior)
        );
        set ("n" & str(anterior)) -> ("n" & str(i))
    };
    
    print("Insertado el nodo: " & str(i));

    i = i + 1
};

END;

GRAPH("g", 20);
insert ("n1", "nodo1");
insert ("n2", "nodo2",below right of "n1");
insert ("n3", "nodo3",below left of "n2");
insert ("n4", "nodo4",above left of "n3");
int i = 1;
while (i < 5) {
    set "n1" <-> ("n" & str(i));
    i = i + 1
};
END

