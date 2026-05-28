
document {
	Key => {bruhatCompare},
	
	Headline => "whether a Coxeter group element is less than another in Bruhat order",
	
	Usage => "bruhatCompare(w, v)",
	
	Inputs => {
		"w" => GroupElement,
		"v" => GroupElement => {"of the same Coxeter group as w"}
		},
	
	Outputs => {
		Boolean => {"whether the first group element is less than or equal to the 
		    second in Bruhat order"}
		},
	
	PARA {"This function is provided by the package ", TO CoxeterGroups,"."},
	
	PARA {"The Bruhat order of a Coxeter group W is the partial order on its elements whose
	    covering relations are precisely the relations w < v where w = v*t for some reflection
	    t of W sand w has smaller length than v.  This function determines w <= v in the Bruhat
	    order for any two elements w and v of W by making use of the fact that w <= v if and
	    only if some reduced subword of v is equal to w."},
	    
	EXAMPLE {
	    	"D = dihedralGroup(4, Variables => {\"u\", \"v\"})",  
		"bruhatCompare(v*u, v*u*v)",
		"bruhatCompare(u*v*u*v, id_D)"
		},
	    
	PARA {"We note that it is not enough to simply check whether the normal form of w is a
	    subword of the normal form of v."},
	    
	EXAMPLE {
	    	"M = matrix {{1, 2, 0}, {2, 1, 0}, {0, 0, 1}}", 
		"W = coxeterGroup({a,b,c}, M)", 
		"bruhatCompare(a*b, b*c*a)",
		"(b*c*a)*(a*c*a)" 
		},	    
	    
	PARA {"For permutations p and q in a symmetric group written in one-line notation as
	    p = p_0p_1...p_(n-1) and q = q_0q_1...q_(n-1), we have p <= q in Bruhat order if
	    and only if the Young tableaux of p and q obtained by sorting the initial strings
	    of lengths determined by the descent set of p have the property that the tableau 
	    of p is entrywise less than or equal to the tableau of q."},
	        
	EXAMPLE {
	    	"S = symmetricGroup 4",
		"bruhatCompare(S_{3,0,2,1}, S_{3,2,1,0})",
		"d = descentSet S_{3,0,2,1}",  
		"tableau(S_{3,0,2,1}, d)",
		"tableau(S_{3,2,1,0}, d)",
		},
	    
	 SeeAlso => {bruhatInterval, bruhatPoset, reflections, tableau}
	    }
	
document {
	Key => {bruhatPoset},
	
	Headline => "construct the Bruhat poset of a finite Coxeter group",
	
	Usage => "bruhatPoset W",
	
	Inputs => {
		"W" => CoxeterGroup => {"a finite group"}
		},
	
	Outputs => {
		Poset => {"of elements of the group ordered by the Bruhat order"}
		},
	
	PARA {"This function is provided by the package ", TO CoxeterGroups,"."},
	
	PARA {"The Bruhat order of a Coxeter group W is the partial order on its elements whose
	    covering relations are precisely the relations w < v where w = v*t for some reflection
	    t of W sand w has smaller length than v.  This function constructs the Bruhat poset
	    of W when W is a finite group."},
	    
	EXAMPLE {
	    	"D = dihedralGroup(4, Variables => {\"u\", \"v\"})",  
		"BPD = bruhatPoset D;",
		"digraph(BPD_*, adjacencyMatrix hasseDiagram BPD)",
		},
	    
	PARA {"Below is an example of the Bruhat poset of a symmetric group."},
	        
	EXAMPLE {
	    	"S = symmetricGroup 4",
		"BPS = bruhatPoset S;",
		"digraph(BPS_*, adjacencyMatrix hasseDiagram BPS)",
		},
	    
	 SeeAlso => {bruhatCompare, bruhatInterval, reflections}
	    }	
