int counter = 0;

GRAPH("grafico1", 5)[3]
    node {"c1"}<0,0>;
    node {"c2"}<0,2>;
    while(counter < 3){
        node{"n" & str(counter)}<counter,1>;
        counter = counter + 1
    };

    counter = 0;

    while(counter < 3){
        if (counter % 2 == 0) {
            edge "brown" str(counter) {"n" & str(counter)} -> {c1}
        } else {
            edge str(counter) {"n" & str(counter)} -> {c2}            
        };
        counter = counter + 1
    }

END