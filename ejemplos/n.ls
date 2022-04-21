GRAPH("n", 3)[4]
    int i = 0;
    while(i < 4){
        string si = str(i);
        node {"A" & si}<i,0>;
        node {"B" & si}<i,1>;
        node {"C" & si}<i,2>;
        node {"D" & si}<i,3>;
        edge str(i) {"A" & si} -> {B0};
        #edge "str(i)" {"A" & si} -> {"B" & si};
        #edge "str(i)" {"B" & si} -> {"C" & si};
        #edge "str(i)" {"C" & si} -> {"D" & si};

        i = i + 1
    }
    # node {"n1"}<0,0>;
    # node {"n2"}<0,1>;
    # edge {n1} <-> {n2}
END