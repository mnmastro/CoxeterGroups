GroupElement = new Type of HashTable
Group = new Type of Monoid
Subgroup = new Type of Monoid
Subgroup.synonym = "subgroup"
CoxeterGroup = new Type of Group
DynkinDiagram = new Type of HashTable


group = method()

group GroupElement := g -> g.group


-----------------------------------------------------------------

generators Group := List => o -> G -> G.generators

id Group := G -> putInGroup({}, G);

length GroupElement := ZZ => g -> #(normalForm g)

numgens Group := ZZ => G -> #(gens G)


-----------------------------------------------------------------

groupOrder = method(Options => {DegreeLimit => 20})

groupOrder GroupElement := ZZ => o -> g -> (
    G := group g;
    i := 1;
    while g^i =!= id_G and i <= o.DegreeLimit do i = i + 1;
    if g^i =!= id_G then error "groupOrder: Degree limit exceeded.  Element may have infinite order.";
    i
    )

groupOrder CoxeterGroup := ZZ => o -> W -> if not isFiniteGroup W then infinity else #(groupElements W) 

-----------------------------------------------------------------

conjugate (GroupElement, GroupElement) := Subgroup => (h, g) -> (
    if not instance(g, group h) then (
	error "Expected a group elements from the same group."
	);
    g*h*g^(-1)
    )

GroupElement ^ GroupElement := GroupElement => (h, g) -> conjugate(h, g)

conjugate (Subgroup, GroupElement) := Subgroup => (H, g) -> (
    G := group H;
    if not instance(g, G) then (
	error "Expected a group element from the same group as the subgroup."
	);
    Hg := new Subgroup from hashTable{
	(symbol generators) => apply(gens H, h -> h^g),
	(symbol group) => G,
        (symbol cache) => new CacheTable from {
	    (symbol relationTables) => H.cache.relationTables,
	    (symbol schriererGraph) => H.cache.schriererGraph,
	    (symbol transversal) => apply(H.cache.transversal, u -> u^g),
	    (symbol DegreeLimit) => H.cache.DegreeLimit, 
	    (symbol CompleteComputation) => H.cache.CompleteComputation
	    }
	};
    
    if H.cache.?isParabolic and H.cache.isParabolic then (
	k := (H.cache.conjugate)*g^(-1);
	Hg.cache.isParabolic = true;
	Hg.cache.conjugate = k;
	Hg.cache.cosetEquals = (u, v) -> H.cache.cosetEquals(u^k, v^k);
	);
    
    Hg
    )

Subgroup ^ GroupElement := Subgroup => (H, g) -> conjugate(H, g)


-----------------------------------------------------------------

commutator = method()

commutator (GroupElement, GroupElement) := GroupElement => (g, h) -> (h^g)*(h^(-1))


-----------------------------------------------------------------

putInGroup = method()

putInGroup (List, Group) := GroupElement => (expr, G) -> (
    nf := reduceWord(expr, G);
    new G from hashTable {
	    (symbol group) => G,
	    (symbol cache) => new CacheTable from {
		(symbol expressions) => unique {nf, expr}
		},
	    (symbol normalForm) => nf
	    }
    )


-- WORDS
-----------------------------------------------------------------

net GroupElement := g -> (
    expr := g.normalForm;
    if #expr == 0 then net (1_ZZ)
    else net concatenate apply(2*#expr - 1, i -> if even i then toString expr#(i//2) else "*")
    )


-----------------------------------------------------------------

expressions = method()

expressions GroupElement := List => g -> g.cache#(symbol expressions)


-----------------------------------------------------------------

wordToGroup = method()

wordToGroup (List, Group) := List => (word, G) -> (
    if #word == 0 then {}
    else apply(word, s -> first select(gens G, t -> normalForm t === {s}) )
    )


-- SUBWORDS
-----------------------------------------------------------------

isSubword = method()

isSubword (List, List) := Boolean => (w, v) -> (
    local pos;
    while #w > 0 do(
	if not isSubset(w, v) then break 
	else (
	    pos = position(v, s -> s == first w);
	    w = drop(w, 1);
	    v = drop(v, pos)
	    )
	);
    if #w == 0 then true else false
    )

isSubword (GroupElement, GroupElement) := Boolean => (w, v) -> (
   if group w =!= group v then (
	error "bruhatCompare: Expected elements of the same group."
	);
    if w == v then true
    else if length w >= length v then false
    else if w == id_(group w) then true
    else any(subwords(v, length w), swd -> swd == w)
    )


-----------------------------------------------------------------

subword = method()

subword (ZZ, GroupElement) := GroupElement => (i, w) -> (
    if abs i > length w then (
	error "subword: Expected an integer at most the length of the group element
	in absolute value."
	);
    putInGroup(take(normalForm w, i), group w)
    )


-----------------------------------------------------------------

subwords = method()

subwords List := List => w -> subsets w

subwords (List, List) := List => (w, v) -> select(subsets v, swd -> isSubword(w, swd) )

subwords (GroupElement, ZZ) := List => (w, l) -> (
    possible := unique apply(flatten apply(length w - l, i -> subsets(normalForm w, l + i) ),
	swd -> putInGroup(swd, group w) );
    -- Any subword that reduces to have length l must have at least l factors

    select(possible, swd -> length swd == l)
    )

--INPUT: A group element w and an integer l at most the length of w
--OUTPUT: A list of all subwords of w of length l

subwords GroupElement := List => w -> unique apply(subwords normalForm w, swd -> putInGroup(swd, group w) )
