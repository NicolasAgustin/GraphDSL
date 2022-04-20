GRAPH("n", 5)[3]
    node {"n1"}<0,0>;
    node {"n2"}<0,1>;
    node {"n3"}<1,0>;
    node {"n4"}<1,1>;
    node {"n5"}<2,2>;

    edge {n1} <- {n2};
    edge {n1} <- {n3};
    edge {n1} <- {n4};
    edge {n1} <- {n5}

END