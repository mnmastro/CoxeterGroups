hasseDiagram' = method()

hasseDiagram' Poset := Digraph => P -> (
    if not P.cache.?coveringRelations then coveringRelations P;
    D := digraph merge(applyPairs(partition(first, P.cache.coveringRelations),
	    (k, v) -> (P.GroundSet#k, sort apply(v, p -> P.GroundSet#(last p) ) ) ), hashTable apply(P.GroundSet, k -> k => {}), join);
    digraph sort edges D
         )


-- BRUHAT ORDER
-----------------------------------------------------------------

bruhatCompare = method()

bruhatCompare (GroupElement, GroupElement) := (w, v) -> (
    if not instance(group v, CoxeterGroup) then (
	error "bruhatCompare: Expected elements of a Coxeter group."
	);
    isSubword(w, v)	    
    )

-- By Theorem 2.2.2 of Bjorner and Brenti, it suffices to check whether some subword
-- of a given reduced word for v is a (reduced) word for w.  Note that the condition of
-- being a reduced subword is not necessary since, if some some subword of v equals w
-- and is not reduced, the Deletion Property implies that some further subword is reduced.


-----------------------------------------------------------------

bruhatInterval = method(Options => {UpperInterval => false})

bruhatInterval (GroupElement, GroupElement) := Poset => o -> (w, v) -> (
    W := group v;
    if group w =!= W then (
	error "bruhatInterval:  Expected elements of the same group."
	);
    if not bruhatCompare(w, v) then (
	error "bruhatInterval: Expected the first group element to be less than or equal to the
	second in Bruhat order."
	);

    poset(select(subwords v, swd -> bruhatCompare(w, swd) ), bruhatCompare)	    
    )

bruhatInterval GroupElement := Poset => o -> w -> (
    if o.UpperInterval then bruhatInterval(w, longWord(group w) )
    else bruhatInterval(id_(group w), w)
    )


-----------------------------------------------------------------

bruhatPoset = method()

bruhatPoset CoxeterGroup := Poset => W -> poset(groupElements W, bruhatCompare)


-- WEAK ORDER
-----------------------------------------------------------------

weakCompare = method(Options => {Left => false})

weakCompare (GroupElement, GroupElement) := o -> (w, v) -> (
    if group w =!= group v then (
	error "weakCompare: Expected group elements of the same Coxeter group."
	);
    if o.Left then (
	isSubset(descentSet(w, AllReflections => true), descentSet(v, AllReflections => true) )
	)
    else (
	isSubset(descentSet(w, Left => true, AllReflections => true), 
	    descentSet(v, Left => true, AllReflections => true) )
	)	    
    )


-----------------------------------------------------------------

weakInterval = method(Options => {Left => false})

weakInterval (GroupElement, GroupElement) := o -> (w, v) -> (
    if not weakCompare(w, v, o) then (
	error "weakInterval: Expected the first group element to be less than or equal to the
	second in weak order."
	);
    W := group w;
    w = wordToGroup(normalForm w, W);
    v = wordToGroup(normalForm v, W);
    poset(apply(subwords(w, v), swd -> wordToGroup(swd, W) ), bruhatCompare)	    
    )


-----------------------------------------------------------------

weakLattice = method(Options => {Left => false})

weakLattice CoxeterGroup := Poset => W -> poset(groupElements W, weakCompare)
