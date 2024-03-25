-- GROUP HOMOMORPHISMS
-----------------------------------------------------------------

GroupMap = new Type of HashTable
GroupMap.synonym = "group map"

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
    W := group w;
    if source f =!= W then (
	error "Expected a group element of the source of the homomorphism."
	);
    f' := hashTable apply(numgens W, i -> (gens W)#i => (targetValues f)#i );
    product apply(wordToGroup(first expressions w, W), s -> f'#s)
    )

-----------------------------------------------------------------

map (CoxeterGroup, CoxeterGroup, List) := GroupMap => o -> (G, W, g) -> (
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
	    (symbol targetValues) => {id_G},
	    (symbol DegreeLimit) => 1, 
	    (symbol CompleteComputation) => false
		}
	    
	};
    f
    ) 

map (CoxeterGroup, List) := GroupMap => o -> (W, g) -> (
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
	    (symbol targetValues) => {id_(R^n)},
	    (symbol DegreeLimit) => 1,  
	    (symbol CompleteComputation) => false
		}
	};
    f
    ) 


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





-- QUOTIENT SETS
-----------------------------------------------------------------

QuotientSet = new Type of HashTable
QuotientSet.synonym = "quotient set"

-*
quotient (CoxeterGroup, Subgroup) := (W, H) -> (
    if group H =!= W then (
	error "quotient: Expected a subgroup of the given Coxeter group."
	);
    WH := new QuotientSet from hashTable {
	(symbol generators) => gens W,
	(symbol group) => W,
	(symbol subgroup) => H,
        (symbol cache) => new CacheTable from {
	    (symbol relationTables) => hashTable apply(relations W, r -> 
		    r => hashTable apply(#r + 1, 
			i ->  i => if i == 0 or i == #r then {1} else {}) 
		    )
		}
	    }
	};
    WH
    )
*-


-- THE TODD-COXETER PROCEDURE
-----------------------------------------------------------------

relationTables = method(Options => {CompleteComputation => true, DegreeLimit => 100, DisplayMode => "data"})

relationTables GroupMap := o -> f -> (
    if o.CompleteComputation and not f.cache.CompleteComputation 
    then toddCoxeterProcedure(f, DegreeLimit => o.DegreeLimit); 
    R := f.cache.relationTables;
    if o.DisplayMode == "pretty" then (
	makeTable := rel -> (
	    header := {flatten apply(#rel, i -> if i == 0 then {" ", rel#i, " "} else {rel#i, " "})};
	    divider := {flatten apply(#rel, i -> if i == 0 then {"_", "_", "_"} else {"_", "_"})};
	    rows :=  apply(values (R#rel), 
		row -> flatten apply(#row, 
		    i -> if i == 0 then {row#i} else {"|", row#i}
		    )
		);
	     netList(header|divider|rows, 
		 Alignment => Center, HorizontalSpace => 1, BaseRow => 0, Boxes => false)
	    );
	netList(apply(sort keys R, rel -> makeTable rel ), 
	    Alignment => Center, HorizontalSpace => 1, BaseRow => 0, Boxes => true)
	)
    else R
    )


-----------------------------------------------------------------

schriererGraph = method(Options => {CompleteComputation => true, DegreeLimit => 100, DisplayMode => "together"})

schriererGraph GroupMap := o -> f -> (
    if o.CompleteComputation and not f.cache.CompleteComputation 
    then toddCoxeterProcedure(f, DegreeLimit => o.DegreeLimit); 
    SG := f.cache.schriererGraph;
    if o.DisplayMode == "separate" then SG else (
	V := toList sum apply(values SG, D -> set vertices D);
	SG = partition(p -> first p, flatten apply(keys SG, 
		s -> apply(edges(SG#s), e -> {e, s}) ) );
	(digraph(V, keys SG), applyValues(SG, v -> v/last) )
	)
    )


-----------------------------------------------------------------

transversal = method(Options => {CompleteComputation => true, DegreeLimit => 100})

transversal GroupMap := List => o -> f -> (
    if o.CompleteComputation and not f.cache.CompleteComputation 
    then toddCoxeterProcedure(f, DegreeLimit => o.DegreeLimit);
    f.cache.transversal
    )


graph Digraph := Graph => o -> D -> graph(vertices D, edges D)

neighbors (Digraph, Thing) := Set => (D, v) -> neighbors(graph D, v) 

-----------------------------------------------------------------

toddCoxeterProcedure = method(Options => {DegreeLimit => 100})

toddCoxeterProcedure GroupMap := o -> f -> (
    W := source f;
    G := target f;
    d := o.DegreeLimit;
    -- if isFiniteGroup W then d = groupOrder W;
    if d > f.cache.DegreeLimit then (
	f.cache.DegreeLimit = d;
    	relTabs := relationTables(f, CompleteComputation => false);
	
	-- find incomplete relation table rows
    	isIncompleteRow := (r, t) -> any(t#r, entry -> entry === ".");
    	isIncompleteTable := t -> any(keys t, r -> isIncompleteRow(r, t));
    	incRelTabs := hashTable select(pairs relTabs, p -> isIncompleteTable last p);
	
	-- update Schrierer graph    
    	updateSchrierer := (s, e) -> (
	    SG := schriererGraph(f, CompleteComputation => false, DisplayMode => "separate");
	    f.cache.schriererGraph = hashTable apply(pairs SG, 
	    	p -> if first p == s then (
		    s => digraph((edges (SG#s))|{e}, EntryMode => "edges")
		    )
	    	else p#0 => p#1
	    	);
	    );
	
	-- update relation table rows
    	updateRelTabs := (relTabs, rel, row, maxRow, tofill) -> (
	    f.cache.relationTables = hashTable apply(pairs relTabs, 
		(r, t) -> r => hashTable (apply(keys t, 
			k -> k => apply(#r + 1, 
			    i -> if r == rel and k == row and i == tofill then maxRow else t#k#i
			    ) 
			)|{maxRow => apply(#r + 1, i -> if i == 0 or i == #r then maxRow else ".") } 
		    )
		) 
	    );


	
	scanRow := (rel, row) -> (
	    iter := 0;
	    newRow := row;
	    newDed := {};
	    SG := schriererGraph(f, CompleteComputation => false, DisplayMode => "separate");
	    while iter <= #row do (
		 newRow = apply(#newRow, 
		     pos -> (
			 if newRow#pos =!= "." then newRow#pos
			 else (
			     s := rel#(pos - 1);
			     i := row#(pos -1);
			     if (set vertices SG#s)#?i then (
				 N := toList neighbors(SG#s, i);
				 if #N > 0 then (
				 j := first N;
				 k := row#(pos + 1);
				 if k =!= "." and not (set edges SG#(rel#pos))#?{j, k}
				then (
				    newDed = newDed|{(rel#pos, {j, k})}
				    );
				first N
				) 
			    else "."
			    )
			else "."
			)
		    )
		);
	    iter = iter + 1;
	    );
	(newRow, newDed)
	);
	
	-- scan relation table rows for deductions
    	scanRelTabs := (relTabs, ded) -> (
	    deductions := {ded};

	    local newDed; local d; local SG;
	    while #deductions > 0 do (
	    	d = first deductions;
	    	updateSchrierer d;

	    	-- scan tables to fill in deductions
		f.cache.relationTables =  hashTable apply(pairs relTabs, 
		    (rel, tab) -> rel => hashTable apply(keys tab, 
			row -> row => (
			    (newRow, newDed) := scanRow(rel, tab#row);
			    deductions = deductions|newDed;
			    newRow
			    )
			)
		    );
		
	   	relTabs = relationTables(f, CompleteComputation => false);
	    	deductions = drop(unique deductions, 1);
	    	);      
	    ); 
	
	local rel; local tab; local incRows; local row; local tofill; local i; local j; local s; 
	local fw; local U; local img; local ded;
    	maxRow := max keys last first pairs relTabs; 
    	while #incRelTabs > 0 and maxRow < d do (
	    maxRow = maxRow + 1;
	    incRows = select(keys last first pairs incRelTabs, 
	    	row -> any(pairs incRelTabs,
		    (rel, tab) -> isIncompleteRow(row, tab) 
		    ) 
	    	);
	    row = min incRows;
	    rel = first sort select(keys incRelTabs, rel -> isIncompleteRow(row, incRelTabs#rel) );
	    tab = incRelTabs#rel; 
	    
	    tofill = position(tab#row, j -> j === ".");
	    i = tab#row#(tofill -1);
	    j = maxRow;
	    U = f.cache.transversal;
	    img = f.cache.targetValues;
	    s = rel#(tofill - 1);
	    fw = (img#i)*(f s);
	    
	    -- look for collapses
	    if (set img)#?fw then (
	    	-- a collapse occurred
	    	j = position(apply(#img, j -> j), j -> img#j == fw);
		updateSchrierer(s, {i, j});
		--print f.cache.schriererGraph;
		ded = (s, {i, j});
		scanRelTabs(relTabs, ded);
	    	relTabs = relationTables(f, CompleteComputation => false);
		ded = (s, {j, i});
		scanRelTabs(relTabs, ded);
	    	relTabs = relationTables(f, CompleteComputation => false);
	    	)
	    else (
	    	-- add to the transversal if no collapse occurred
	    	f.cache.targetValues = img|{fw};
	    	f.cache.transversal = U|{(U#i)*s};
	    	-- adds new rows to relation tables
		updateSchrierer(s, {i, j});
		--print f.cache.schriererGraph;
	    	updateRelTabs(relTabs, rel, row, j, tofill);
	    	relTabs = relationTables(f, CompleteComputation => false);
	    	--print relTabs;
		
		if tofill == #rel - 1 then (
		    ded = (s, {maxRow, row});
		    scanRelTabs(relTabs, ded);
	    	    relTabs = relationTables(f, CompleteComputation => false);
		    )
		 else if relTabs#rel#row#(tofill + 1) =!= "." then (
		    ded = (rel#tofill, {maxRow, tab#row#(tofill + 1)});
		    scanRelTabs(relTabs, ded);
	    	    relTabs = relationTables(f, CompleteComputation => false);
		    );
		);

	    incRelTabs = hashTable select(pairs relTabs, p -> isIncompleteTable last p);
	    --print incRelTabs;
	    );

	if #incRelTabs == 0 then f.cache.CompleteComputation = true;
	)
    ) 


	-*
	rel = first sort keys incRelTabs;
	tab = incRelTabs#rel;
	incRows = select(keys tab, row -> isIncompleteRow(row, tab));
	row = first sort incRows;
	*-

-----------------------------------------------------------------

quotientMap = method()

quotientMap GroupMap := f -> (
    U := transversal f;
    q := hashTable apply(#U, i ->  f.cache.targetValues#i => U#i); 
    w ->  (
	if not instance(w, source f) then (
	    error "quotientMap: Expected an element of the source of the homomorphism."
	    );
	q#(f w)
	)
    )

-----------------------------------------------------------------

kernel GroupMap := Subgroup => o -> f -> (
    if not f.cache.CompleteComputation then toddCoxeterProcedure f;
    q := quotientMap f;
    U := transversal f;
    W := source f;
    K := new Subgroup from hashTable{
	(symbol generators) => unique flatten apply(U, u -> 
	    apply(select(apply(gens W, s -> u*s), w -> not (set U)#?w), w -> w*(q w)^(-1) )
	    ),
	(symbol group) => W,
        (symbol cache) => new CacheTable from {
	    (symbol relationTables) => relationTables f,
	    (symbol transversal) => U,
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

-*
toddCoxeterProcedure QuotientSet := X -> (
    relTabs := relationTables X;
    rels := keys relTabs;
    tabs := values relTabs;
    isIncompleteTable := t -> (
	trows := values t;
	any(trows, r -> #r =! #(first trows) )
	);
    incTabs := select(tabs, t -> isIncompleteTable t);
   
    local tab;
    while #incTabs > 0 do (
	tab = first incTab;
	
	
	)
    
    )
*- 

