document {
	Key => {group},
	
	Headline => "get the associated group of an object",
	
	Usage => "group w",
	
	Inputs => {
		"w" => GroupElement
		},
	
	Outputs => {
		CoxeterGroup => {"the group associated to the input object"}
		},
	
	PARA {"This function is provided by the package ", TO CoxeterGroups,"."},
	
	EXAMPLE {
	    	"D = dihedralGroup 4", 
		"gens D", 
	    	"group s"
		},
	    
	 SeeAlso => {coxeterGroup}
	    }

document {
	Key => {groupOrder, [groupOrder, DegreeLimit]},
	
	Headline => "the order of a group or group element",
	
	Usage => "groupOrder u \n groupOrder W",
	
	Inputs => {
	    	"u" => GroupElement,
		"W" => CoxeterGroup,
		DegreeLimit => ZZ => {"the maximum exponent to check whether the element has order less than or equal to"}
		},
	
	Outputs => {
		ZZ => {"the order of the group or group element"}
		},
	
	PARA {"This function is provided by the package ", TO CoxeterGroups,"."},
	
	EXAMPLE {
	    	"groupOrder symmetricGroup 4",
	    	"D = dihedralGroup 4",
		"r = s*t", 
		"groupOrder r"
		},
	    
	 SeeAlso => {coxeterGroup, isFiniteGroup}
	    }

document {
	Key => {(length, GroupElement)},
	
	Headline => "the length of a Coxeter group element",
	
	Usage => "length w",
	
	Inputs => {
		"w" => GroupElement
		},
	
	Outputs => {
		ZZ => {"the length of the group element"}
		},
	
	PARA {"This function is provided by the package ", TO CoxeterGroups,"."},
	
	PARA {"Given a Coxeter group W with set of generators S, the length of a group element w
	    is the smallest number of factors in a word on the generators S whose product in W is
	    equal to w.  A word representing w of minimum length is said to be reduced.  Since all
	    group elements are automatically represented by a canonical reduced word in the 
	    generators, the length of w is just the number of factors in this reduced expression."},
	    
	EXAMPLE {
	    	"D = dihedralGroup(4, Variables => {\"u\", \"v\"})",  
		"r = u*v",
		"length(r^3)",
		"r^3"
		},
	    
	PARA {"For a permutation p in a symmetric group, the length of p is equal to the number of
	    inversions of p when written in one-line notation p = p_1p_2...p_n.  An inversion of p
	    is any pair of indices i < j such that p_i > p_j."},
	        
	EXAMPLE {
	    	"S = symmetricGroup 4",  
		"p = s_0*s_2*s_1*s_2",
		"length p"
		},
	    
	PARA {"The one-line notation of the permutation p can be obtained as follows."},
	        
	EXAMPLE {
		"toList p"
		},
	    
	 SeeAlso => {coxeterGroup}
	    }


document {
	Key => {(subword, ZZ, GroupElement)},
	
	Headline => "get a subword of a group element",
	
	Usage => "subword(i, w)",
	
	Inputs => {
	        "i" => ZZ,
		"w" => GroupElement
		},
	
	Outputs => {
		GroupElement => {"a subword of the group element"},
		},
	
	PARA {"This function is provided by the package ", TO CoxeterGroups,"."},
	
	PARA {"Given a group element w with normal form w = s_1*s_2*...*s_n an integer i at most n, 
	    this function returns the initial segment subword s_1*s_2*...*s_i of length i."},
	    
	EXAMPLE {
	    	"S = symmetricGroup 5",
		"w = S_{1, 2, 3, 4, 0}",  
		"subword(3, w)"
		},
	    
	PARA {"If i is negative, then this function returns a subword consisting of the last i factors
	    of the normal form of w instead."},   

	EXAMPLE {
	    	"subword(-2, w)"
		},
	    
	 SeeAlso => {isSubword, (subwords, GroupElement)}
	    }

document {
	Key => {(subwords, GroupElement)},
	
	Headline => "get a list of all subwords of a group element",
	
	Usage => "subwords w",
	
	Inputs => {
		"w" => GroupElement
		},
	
	Outputs => {
		List => {"all subwords of the normal form of a group element"},
		},
	
	PARA {"This function is provided by the package ", TO CoxeterGroups,"."},
	
	PARA {"Given a group element w with normal form w = s_1*s_2*...*s_n, a subword of w
	    is any expression of the form s_{i_1}*s_{i_2}*...*s_{i_m} with 1 <= i_1 < i_2 < ... < i_m <= n.
	    This function returns a list of all subwords of the normal form of w."},
	    
	EXAMPLE {
	    	"S = symmetricGroup 4",
		"w = S_{2, 3, 0, 1}",  
		"subwords w"
		},
	    
	 SeeAlso => {isSubword, (subword, ZZ, GroupElement)}
	    }

document {
	Key => {(subwords, GroupElement, ZZ)},
	
	Headline => "get a list of all subwords of a group element of a given length",
	
	Usage => "subwords w",
	
	Inputs => {
		"w" => GroupElement,
		"l" => ZZ => "a non-negative integer at most the length of the group element"
		},
	
	Outputs => {
		List => {"all subwords of the normal form of the group element of the given length"},
		},
	
	PARA {"This function is provided by the package ", TO CoxeterGroups,"."},
	
	PARA {"Given a group element w with normal form w = s_1*s_2*...*s_n, a subword of w
	    is any expression of the form s_{i_1}*s_{i_2}*...*s_{i_m} with 1 <= i_1 < i_2 < ... < i_m <= n.
	    This function returns a list of all subwords of the normal form of w that reduce to have length l."},
	    
	EXAMPLE {
	    	"S = symmetricGroup 4",
		"w = S_{2, 3, 0, 1}",  
		"subwords(w, 2)"
		},
	    
	 SeeAlso => {isSubword, (subword, ZZ, GroupElement)}
	    }
