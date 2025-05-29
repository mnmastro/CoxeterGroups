
-- GROUP HOMOMORPHISMS
-----------------------------------------------------------------

target GroupMap := f -> f.target
source GroupMap := f -> f.source

targetValues = method()

targetValues GroupMap := f -> f.targetValues

expression GroupMap := f -> (expression map) (expression (target f, source f, targetValues f))
net GroupMap := f -> net expression f

GroupMap#{Standard,AfterPrint} = GroupMap#{Standard,AfterNoPrint} = f -> (
     << endl;				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : " << class f << " " <<
     horizontalJoin(net target f, " <--- ", net source f) << endl;
     )
-- Magic code for having Macaulay2 print the source and target of the map.


-----------------------------------------------------------------

GroupMap * GroupMap := (f, g) -> (
    if target g =!= source f then (
	error "Homomorphisms cannot be composed."
	);
    if instance(target f, CoxeterGroup) then (
	map(target f, source g, apply(targetValues g, v -> f v) )
	)
    else (
	map(source g, apply(targetValues g, v -> f v) )
	)
    )

GroupMap GroupElement := (f, w) -> (
    W := source f;
    if not instance(w, W) then (
	error "Expected a group element of the source of the homomorphism."
	);
    f' := hashTable apply(numgens W, i -> (gens W)#i => (targetValues f)#i );
    word := wordToGroup(first expressions w, W);
    if #word == 0 then (
	G := target f;
	if instance(G, CoxeterGroup) then id_G
	else (
	    R := ring first targetValues f;
	    n := numRows first targetValues f;
	    id_(R^n)
	    )
	)
    else product apply(word, s -> f'#s)
    )

-----------------------------------------------------------------

map (Group, Group, List) := GroupMap => o -> (G, W, g) -> (
    if #g =!= numgens W then (
	error "map: Expected the list to have as many elements as the generators of the second
	Coxeter group."
	);
    if any(g, u -> not instance(u, G) ) then (
	error "map: Expected a list of elements of the first Coxeter group."
	); 
    f := hashTable apply(#g, i -> (gens W)#i => g#i);
    if any(relations W, r -> product apply(r, s -> f#s) =!= id_G) then (
	error "map: The given list does not define a group homomorphism between the given 
	Coxeter groups."
	);
    f = new GroupMap from hashTable {
	(symbol source) => W,
	(symbol target) => G,
	(symbol targetValues) => g,
	(symbol cache) => new CacheTable from {
	    (symbol relationTables) => hashTable apply(relations W, r -> 
		    r => hashTable {0 => apply(#r + 1, 
			    i -> if i == 0 or i == #r then 0 else ".")} 
		    ),
	    (symbol schriererGraph) => hashTable apply(gens W, s -> s => digraph({0}, {}) ),
	    (symbol transversal) => {id_W},
	    (symbol DegreeLimit) => 1, 
	    (symbol CompleteComputation) => false
		}
	    
	};
    f
    ) 

map (Group, Group) := GroupMap => o -> (G, W) -> (
    Wsymb := W#generatorSymbols;
    Gsymb := G#generatorSymbols;
    g := apply(numgens W, i -> 
	if any(Gsymb, h -> h === Wsymb#i) then (gens G)#(position(Gsymb, h -> h ===  Wsymb#i)) 
	else id_G
	);
    map(G, W, g)
    )

map (Group, List) := GroupMap => o -> (W, g) -> (
    if #g =!= numgens W then (
	error "map: Expected the list to have as many elements as the generators of the 
	Coxeter group."
	);
    if any(g, u -> not instance(u, Matrix) ) then (
	error "map: Expected a list of matrices."
	);
    n := numRows first g;
    if any(g, u -> numRows u =!= n or numColumns u =!= n ) then (
	error "map: Expected a list of square matrices of the same size."
	); 
    R := ring first g;
    if any(g, u -> ring u =!= R ) then (
	error "map: Expected a list of matrices over the same ring."
	); 
    f := hashTable apply(#g, i -> (gens W)#i => g#i);
    if any(relations W, r -> product apply(r, s -> f#s) =!= id_(R^n)) then (
	error "map: The given list does not define a group homomorphism from the given 
	Coxeter group."
	);
    f = new GroupMap from hashTable {
	(symbol source) => W,
	(symbol target) => "GL("|toString(n)|", "|toString(R)|")",
	(symbol targetValues) => g,
	(symbol cache) => new CacheTable from {
	    (symbol relationTables) => hashTable apply(relations W, r -> 
		    r => hashTable {0 => apply(#r + 1, 
			    i -> if i == 0 or i == #r then 0 else ".")} 
		    ),
	    (symbol schriererGraph) => hashTable apply(gens W, s -> s => digraph({0}, {}) ),
	    (symbol transversal) => {id_W},
	    (symbol DegreeLimit) => 1,  
	    (symbol CompleteComputation) => false
		}
	};
    f
    ) 


-----------------------------------------------------------------

substitute (GroupElement, Group) := GroupElement => (w, G) -> (map(G,group w)) w


-----------------------------------------------------------------

GroupElement % GroupMap := (w, f) -> (quotientMap f) w

GroupElement % Subgroup := (w, H) -> (quotientMap H) w


-----------------------------------------------------------------


grpKerOpts := {DegreeLimit => 100};
kernel GroupMap := Subgroup => grpKerOpts -> f -> (
    if not f.cache.CompleteComputation then toddCoxeterProcedure f;
    q := quotientMap f;
    U := transversal f;
    W := source f;
    K := sort unique flatten apply(U, 
	u -> apply(select(apply(gens W, s -> u*s), w -> not (set U)#?w), 
	    w -> w*(q w)^(-1) 
	    )
	);
    gensK := {};
    scan(K, k -> if all(gensK, h -> h =!= k^(-1)) then gensK = gensK|{k});
    K = new Subgroup from hashTable{
	(symbol generators) => gensK,
	(symbol group) => W,
        (symbol cache) => new CacheTable from {
	    (symbol relationTables) => relationTables f,
	    (symbol transversal) => U,
	    (symbol isNormal) => true,
	    (symbol CompleteComputation) => true
	    }
	};
    K
    )


-----------------------------------------------------------------

image GroupMap := Subgroup => f -> (
    W':= new Subgroup from hashTable{
	(symbol generators) => targetValues,
	(symbol group) => target f,
        (symbol cache) => new CacheTable from {
	    (symbol relationTables) => relationTables f,
	    (symbol CompleteComputation) => true
	    }
	};
    W'
    )


-- SPECIFIC HOMOMORPHISMS
-----------------------------------------------------------------

reflectionRepresentation = method()

reflectionRepresentation CoxeterGroup := GroupMap => W -> map(W, reflectionRepresentatives W)


-----------------------------------------------------------------

permutationRepresentation = method()

permutationRepresentation CoxeterGroup := GroupMap => W -> (
    if not W.cache#?hasType or #(W.cache#hasType) =!= 2 or first W.cache#hasType =!= "A" then (
	error "permutationRepresentation: Expected a Coxeter group of type \"A\"."
	);
    n := numgens W + 1;    
    map(W, apply(gens W, s -> (id_(ZZ^n))_(toList s) ) )
    )


-----------------------------------------------------------------

regularEmbedding = method(Options => {Left => true})

regularEmbedding CoxeterGroup := GroupMap => o -> W -> (
    if not isFiniteGroup W then (
	error "regularEmbedding: Expected a finite Coxeter group."
	);
    N := groupOrder W;
    S := symmetricGroup N;
    grp := groupElements W;
    map(S, W, apply(gens W, s -> (
		p := apply(#grp, 
		    i -> position(grp, w -> w == s*(grp#i) )
		    );
		S_p
		)
	    )
	)
    )


-----------------------------------------------------------------
		
regularRepresentation = method(Options => {Left => true})

regularRepresentation CoxeterGroup := GroupMap => o -> W -> (
    f := regularEmbedding W;
    S := target f;
    p := permutationRepresentation S;
    p * f
    )


-----------------------------------------------------------------

signMap = method()

signMap CoxeterGroup := GroupMap => W -> map(W, apply(numgens W, i -> matrix {{-1}}) )

