Coset = new Type of HashTable
GroupMap = new Type of HashTable
GroupMap.synonym = "group map"
QuotientSet = new Type of Monoid
QuotientSet.synonym = "quotient set"

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

relationTables Subgroup := o -> H -> (
    if o.CompleteComputation and not H.cache.CompleteComputation 
    then toddCoxeterProcedure(H, DegreeLimit => o.DegreeLimit); 
    R := H.cache.relationTables;
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

schriererGraph = method(Options => {
	CompleteComputation => true, 
	DegreeLimit => 100,
	Cosets => Left, 
	DisplayMode => "together"})

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

schriererGraph Subgroup := o -> H -> (
    if o.CompleteComputation and not H.cache.CompleteComputation 
    then toddCoxeterProcedure(H, DegreeLimit => o.DegreeLimit, Cosets => o.Cosets); 
    SG := H.cache.schriererGraph;
    if o.DisplayMode == "separate" then SG else (
	V := toList sum apply(values SG, D -> set vertices D);
	SG = partition(p -> first p, flatten apply(keys SG, 
		s -> apply(edges(SG#s), e -> {e, s}) ) );
	(digraph(V, keys SG), applyValues(SG, v -> v/last) )
	)
    )


-----------------------------------------------------------------

transversal = method(Options => {CompleteComputation => true, DegreeLimit => 100, Cosets => Left})

transversal GroupMap := List => o -> f -> (
    if o.CompleteComputation and not f.cache.CompleteComputation 
    then toddCoxeterProcedure(f, DegreeLimit => o.DegreeLimit);
    f.cache.transversal
    )

transversal Subgroup := List => o -> H -> (
    if o.CompleteComputation and not H.cache.CompleteComputation 
    then toddCoxeterProcedure(H, DegreeLimit => o.DegreeLimit, Cosets => o.Cosets);
    H.cache.transversal
    )


-----------------------------------------------------------------

graph Digraph := Graph => o -> D -> graph(vertices D, edges D)

neighbors (Digraph, Thing) := Set => (D, v) -> neighbors(graph D, v) 

-----------------------------------------------------------------

toddCoxeterProcedure = method(Options => {DegreeLimit => 100, Cosets => Left})

toddCoxeterProcedure (List, Function, ZZ) := o -> (data, coll, d) -> (
    relTabs := data#0;
    SG := data#1;
    U := data#2;
    
    -- find incomplete relation table rows
    isIncompleteRow := (r, t) -> any(t#r, entry -> entry === ".");
    isIncompleteTable := t -> any(keys t, r -> isIncompleteRow(r, t));
    incRelTabs := hashTable select(pairs relTabs, p -> isIncompleteTable last p);
    
    -- update Schrierer graph    
    updateSchrierer := (s, e) -> (
	SG = hashTable apply(pairs SG, 
	    p -> (
		if first p == s then (
		    s => digraph((edges (SG#s))|{e}, EntryMode => "edges")
		    )
		else p#0 => p#1
		)
	    );
	--print (s, e, SG);
	SG
	);
    
    -- update relation table rows
    updateRelTabs := (relTabs, rel, row, maxRow, tofill) -> (
	relTabs = hashTable apply(pairs relTabs, 
	    (r, t) -> r => hashTable (apply(keys t, 
		    k -> k => apply(#r + 1, 
			i -> (if r == rel and k == row and i == tofill then maxRow else t#k#i) )
		    )|{maxRow => apply(#r + 1, i -> if i == 0 or i == #r then maxRow else ".") } 
		) 
	    );
	relTabs
	);
    
    scanRow := (rel, row) -> (
	iter := 0;
	newRow := row;
	newDed := {};
	local s; local t; local i; local j; local k; local N;
	
	-- forward scanning
	while iter <= #row do (
	    newRow = apply(#newRow, 
		pos -> (
		    if newRow#pos =!= "." then newRow#pos
		    else (
			s = rel#(pos - 1);
			t = rel#pos;
			i = row#(pos - 1);
			k = row#(pos + 1);
			if i =!= "." then (
			    -- check whether s: i -> ? is known
			    N = select(edges SG#s, e -> first e == i);
			    if #N > 0 then (
				j = first (N/last);
				-- look for new deductions
				if k =!= "." and not (set edges SG#t)#?{j, k}
				then (
				    newDed = newDed|{(t, {j, k})}
				    );
			    	j
			    	)
			    else "."
			    )
			else "."
			) 
		    )
		);
	    --print newRow;
	    iter = iter + 1;
	    );
	
	iter = 0;
	-- back scanning
	newRow = reverse newRow;
	rel' := reverse rel;
	while iter <= #row do (
	    newRow = apply(#newRow, 
		pos -> (
		    if newRow#pos =!= "." then newRow#pos
		    else (
			s = rel'#(pos - 1);
			t = rel'#pos;
			i = newRow#(pos - 1);
			k = newRow#(pos + 1);
			if i =!= "." then (
			    -- check whether s: ? -> i is known
			    N = select(edges SG#s, e -> last e == i);
			    if #N > 0 then (
				j = first (N/first);
				-- look for new deductions
				if k =!= "." and not (set edges SG#t)#?{k, j}
				then (
				    newDed = newDed|{(t, {k, j})}
				    );
				j
				)
			    else "."
			    )
			else "."
			)
		    )
		);
	    iter = iter + 1;
	    );
	(reverse newRow, newDed)
	);
    
    -- scan relation table rows for deductions
    scanRelTabs := (relTabs, ded) -> (
	deductions := {ded};
	
	local newDed; local d;
	while #deductions > 0 do (
	    d = first deductions;
	    SG = updateSchrierer d; 
	    
	    -- scan tables to fill in deductions
	    relTabs =  hashTable apply(pairs relTabs, 
		(rel, tab) -> rel => hashTable apply(keys tab, 
		    row -> row => (
			(newRow, newDed) := scanRow(rel, tab#row);
			deductions = deductions|newDed;
			newRow
			)
		    )
		);
	    
	    deductions = drop(unique deductions, 1);
	    );
	relTabs      
	); 
    
    local rel; local tab; local incRows; local row; local tofill; local i; local s; 
    local w; local ded;
    iter := 0;
    j := max keys last first pairs relTabs; 
    while #incRelTabs > 0 and iter < d do (
	iter = iter + 1;
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
	j = (max keys tab) + 1;
	s = rel#(tofill - 1);
	w = s*(U#i);
	
	-- look for collapses
	if any(U, u -> coll(u, w) ) then (
	    -- a collapse occurred
	    j = position(U, u -> coll(u, w) );
	    SG = updateSchrierer(s, {i, j});
	    --print SG;
	    ded = (s, {i, j});
	    relTabs = scanRelTabs(relTabs, ded);
	    ded = (s, {j, i});
	    relTabs = scanRelTabs(relTabs, ded);
	    )
	else (
	    -- add to the transversal if no collapse occurred
	    U = U|{w};
	    -- adds new rows to relation tables
	    SG = updateSchrierer(s, {i, j});
	    --print SG;
	    relTabs = updateRelTabs(relTabs, rel, row, j, tofill);
	    ded = (s, {i, j});
	    relTabs = scanRelTabs(relTabs, ded);
	    --print relTabs;
	    
	    );
	
	incRelTabs = hashTable select(pairs relTabs, p -> isIncompleteTable last p);
	--print incRelTabs;
	);
    
    finished := if #incRelTabs == 0 then true else false;
    ({relTabs, SG, U}, finished)
    )
 

toddCoxeterProcedure GroupMap := o -> f -> (
    d := o.DegreeLimit - f.cache.DegreeLimit;
    -- if isFiniteGroup W then d = groupOrder W;
    if d > 0 then (
	f.cache.DegreeLimit = o.DegreeLimit;
    	relTabs := relationTables(f, CompleteComputation => false);
	SG := schriererGraph(f, CompleteComputation => false, DisplayMode => "separate");
	U := f.cache.transversal;
	coll := (u, w) -> f u == f w;
	
	(data, finished) := toddCoxeterProcedure({relTabs, SG, U}, coll, d);
	f.cache.relationTables = data#0;
	f.cache.schriererGraph = data#1;
	f.cache.transversal = data#2;
	f.cache.CompleteComputation = finished;
	)	
    ) 

toddCoxeterProcedure Subgroup := o -> H -> (
    if not isParabolic H then (
	error "toddCoxeterProcedure: Not yet implemented for non-parabolic subgroups"
	);
    d := o.DegreeLimit - H.cache.DegreeLimit;
    -- if isFiniteGroup W then d = groupOrder W;
    if d > 0 then (
	H.cache.DegreeLimit = o.DegreeLimit;
    	relTabs := relationTables(H, CompleteComputation => false);
	SG := schriererGraph(H, CompleteComputation => false, DisplayMode => "separate");
	U := H.cache.transversal;
	coll := H.cache.cosetEquals;
	if o.Cosets == Right then (
	    coll = (u, w) -> coll(u^(-1), w^(-1))
	    );
	
	(data, finished) := toddCoxeterProcedure({relTabs, SG, U}, coll, d);
	H.cache.relationTables = data#0;
	H.cache.schriererGraph = data#1;
	H.cache.transversal = data#2;
	H.cache.CompleteComputation = finished;
	)	
    ) 

-*
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
*-


-----------------------------------------------------------------

quotientMap = method(Options => {Cosets => Left})

quotientMap GroupMap := o -> f -> (
    U := transversal f;
    w ->  (
	if not instance(w, source f) then (
	    error "quotientMap: Expected an element of the source of the homomorphism."
	    );
	first select(U, u -> f u == f w)
	)
    )

quotientMap Subgroup := o -> H -> (
    if not H.cache.?cosetEquals then (
	error "quotientMap: Expected subgroup to have a function to check for membership."
	);
    coll := H.cache.cosetEquals;
    if o.Cosets == Right then (
	coll = (u, w) -> coll(u^(-1), w^(-1))
	);
    U := transversal(H, o);
    w ->  (
	if not instance(w, group H) then (
	    error "quotientMap: Expected an element of the group of the subgroup."
	    );
	first select(U, u -> coll(u, w) )
	)
    )

-----------------------------------------------------------------

GroupElement % GroupMap := (w, f) -> (quotientMap f) w

GroupElement % Subgroup := (w, H) -> (quotientMap H) w


-- PRESENTATIONS
-----------------------------------------------------------------

presentation CoxeterGroup := Divide => W -> (
    F := net "F("|
	net concatenate drop(drop(characters toString apply(W.generators, s -> net s), 1 ), -1)
    	|")";
    m := coxeterMatrix W;
    S := gens W;
    R := flatten apply(numgens W, i -> apply(i + 1, j -> {S#j, S#i, m_j_i} ) );
    R = toSequence apply(select(R, r -> (last r) > 0), 
	r -> new Power from if (last r) == 1 then {r#0, 2} else {r#0*r#1, r#2}
	);
    new Divide from { expression F, expression R }
    )


-- QUOTIENT SETS
-----------------------------------------------------------------

net Coset := C -> (
    H := subgroup C;
    hasAttribute := value Core#"private dictionary"#"hasAttribute";
    getAttribute := value Core#"private dictionary"#"getAttribute";
    ReverseDictionary := value Core#"private dictionary"#"ReverseDictionary";
    if hasAttribute(H,ReverseDictionary) then (
	netH := net toString getAttribute(H,ReverseDictionary);
	(net C.normalForm)|(netH)
	)
    else (
	(net C.normalForm)|(net H)
	)
    )


lift Coset := GroupElement => o -> C -> C.normalForm
quotient Coset := QuotientSet => o -> C -> C.quotient
subgroup Coset := Subgroup => C -> subgroup quotient C
group Coset := CoxeterGroup => C -> group quotient C

-----------------------------------------------------------------

GroupElement Subgroup := Coset => (w, H) -> (
    X := quotient H;
    (quotientMap X) w
    )


-----------------------------------------------------------------

new QuotientSet from List := (QuotientSet, inits) -> new QuotientSet of Coset from new HashTable from inits

--grpQuotOpts := {Cosets => Left}
quotient Subgroup :=  QuotientSet => o -> H -> (
    --o := grpQuotOpts;
    --if o.Cosets =!= Left and o.Cosets =!= Right then (
	--error "quotient: Must have either left or right cosets."
	--);
    W := group H;
	
    X := new QuotientSet from {
	(symbol group) => group H,
	(symbol subgroup) => H,
	(symbol quotientMap) => {},
	(symbol groundSet) => {},
	--(symbol Cosets) => o.Cosets,
	cache => new CacheTable from {}
	};
    
    p := hashTable apply(transversal H, --Cosets => o.Cosets
	    u -> u => new X from hashTable {
		(symbol quotient) => X,
	    	(symbol cache) => new CacheTable from {},
       	    	(symbol normalForm) => u
            	}
	    );
    X#(symbol groundSet) = set values p;
    
    X#(symbol quotientMap) = w -> p#((quotientMap H) w);
    
    X ? X := (C, C') -> C.normalForm ? C'.normalForm;
    
    
    W * X := (w, C) -> (
	p := X.quotientMap;
	p(w * (lift C))
	);
    
    X
    )


CoxeterGroup / Subgroup := (W, H) -> (
    if group H =!= W then (
	error "Expected a subgroup of the given Coxeter group."
	);
    quotient H
    )

Subgroup \ CoxeterGroup := (H, W) -> (
    if group H =!= W then (
	error "Expected a subgroup of the given Coxeter group."
	);
    quotient(H, Cosets => Right)
    )

-----------------------------------------------------------------

group QuotientSet := CoxeterGroup => X -> X.group
net QuotientSet := X -> (
    H := subgroup X;
    hasAttribute := value Core#"private dictionary"#"hasAttribute";
    getAttribute := value Core#"private dictionary"#"getAttribute";
    ReverseDictionary := value Core#"private dictionary"#"ReverseDictionary";
    if hasAttribute(H,ReverseDictionary) then (
	netH := net toString getAttribute(H,ReverseDictionary);
	(net group X)|"/"|(netH)
	)
    else (
	(net group X)|"/"|(net H)
	)
    )

groundSet QuotientSet := X ->  X.groundSet
subgroup QuotientSet := Subgroup => X -> X.subgroup
quotientMap QuotientSet := Function => o -> X -> X.quotientMap


isSubset (Coset, Coset) := Boolean => (C, C') -> (
    H' := subgroup quotient C';
    w' := C'.normalForm;
    isSubset((w')^(-1)*C, H')
    )

-*
isSubset (Coset, Subgroup) := Boolean => (C, H') -> (
    H := subgroup quotient C;
    w := C.normalForm;
    all(apply(gens H, h -> w*h), g -> g%H' == id_W)
    )
*-
