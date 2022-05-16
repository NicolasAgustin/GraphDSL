GRAPH("test", 5)[2]
    node {"n1"}<0,0>;
    node {"n2"}<0,1>;
    edge "red" "2" {n1} -> {n1};
    edge "red" "3" {n1} -> {n2}
END