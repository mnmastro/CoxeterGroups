
graph MonomialIdeal := Graph => I -> (
    expos := flatten(I_*/exponents);
    if any(expos, e -> sum e =!= 2) then (
	error "graph: Expected a quadratic monomial ideal."
	);
    if any(expos, e -> any(e, d -> d > 1) ) then (
	    error "graph: Expected a squarefree monomial ideal."
	    );
    graph apply(expos, e -> positions(e, i -> i == 1) )
    )
	
