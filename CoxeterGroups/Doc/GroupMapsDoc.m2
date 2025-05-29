document {
	Key => {(kernel, GroupMap)},
	
	Headline => "compute the kernel of a group homomorphism",
	
	Usage => "kernel f \n ker f",
	
	Inputs => {
		"f" => GroupMap
		},
	    
	Outputs => {
	    Subgroup => {"the kernel of the homomorphism"}
	    },
	
	PARA {"This function is provided by the package ", TO CoxeterGroups,"."},
	
	PARA {"This function computes a set of generators for the kernel of a group homomorphism
	    via the Todd-Coxeter procedure."},
	
	PARA {"The kernel of the sign homomorphism on the symmetric group on 4 letters is the 
	    alternating group on 4 letters."},
	
	EXAMPLE {
	    "S = symmetricGroup 4",
	    "sgn = signMap S",
	    "A = ker sgn"
	    },

	PARA {"The dihedral group of symmetries of a square admits a homomorphism to the Klein 4 group.
	    We compute the kernel of this homomorphism."},
	
	EXAMPLE {
	     "C2 = symmetricGroup 2",
	     "V = C2 * C2",
	     "D = dihedralGroup 4",
	     "f = map(V, D, {V_0, V_1})",
	     "ker f" 
	     },
	    
	 SeeAlso => {(image, GroupMap), (map, Group, List), (map, Group, Group), (map, Group, Group, List), toddCoxeterProcedure}
	    }

document {
	Key => {(map, Group, Group)},
	
	Headline => "construct an induced group homomorphism",
	
	Usage => "map(H, G)",
	
	Inputs => {
	    "H" => Group,
	    "G" => Group
	    },
	
	   
	Outputs => {
	    GroupMap => {"the natural induced homomorphism from G to H"}
	    },
	
	
	PARA {"This function is provided by the package ", TO CoxeterGroups,"."},
	
	PARA {"This function creates a homorphism between the given groups by by sending each generator of G to 
	    the genererator of H of the same name if possible or to the identity element otherwise."},

	PARA {"The dihedral group of symmetries of an octagon admits an induced homomorphism to the dihedral group
	    of symmetries of a square."},
	
	EXAMPLE {
	    "G = dihedralGroup 8",
	    "H = dihedralGroup 4",
	    "f = map(H, G)",
	    "ker f"
	     },
	    
	 SeeAlso => {(kernel, GroupMap), (image, GroupMap), (map, Group, List), (map, Group, Group, List), toddCoxeterProcedure}
	    }

document {
	Key => {(map, Group, Group, List)},
	
	Headline => "construct a group homomorphism",
	
	Usage => "map(H, G, L)",
	
	Inputs => {
	    "H" => Group,
	    "G" => Group,
	    "L" => List => "a list of elements of H representing the values of the generators of G"
	    },
	   
	Outputs => {
	    GroupMap => {"the group homomorphism from G to H determined by the entries of L"}
	    },
	
	
	PARA {"This function is provided by the package ", TO CoxeterGroups,"."},

	PARA {"The dihedral group of symmetries of a square admits a homomorphism to the Klein 4 group.
	    We construct this homomorphism."},
	
	EXAMPLE {
	     "C2 = symmetricGroup 2",
	     "V = C2 * C2",
	     "D = dihedralGroup 4",
	     "f = map(V, D, {V_0, V_1})",
	     "ker f"
	     },
	    
	 SeeAlso => {(kernel, GroupMap), (image, GroupMap), (map, Group, List), (map, Group, Group), toddCoxeterProcedure}
	    }

document {
	Key => {(map, Group, List)},
	
	Headline => "construct a group homomorphism to a matrix group",
	
	Usage => "map(G, m)",
	
	Inputs => {
	    "G" => Group,
	    "m" => List => "a list of invertible matrices representing the images of the generators of the group"
	    },
	   
	Outputs => {
	    GroupMap => {"the map from G to GL_n determined by the given list of matrices"}
	    },
	
	
	PARA {"This function is provided by the package ", TO CoxeterGroups,"."},
	
	PARA {"This function constructs the homomorphism from a given group G to GL_n by mapping the generators of G in order
	    to the elements of GL_n from the given list of matrices.  If the given matrices have entries in a field k, then such
	    a homomorphism is equivalent to specifying a representation of G on k^n."},

	PARA {"We construct the natural representation of the dihedral group of index 4 on the plane that acts on the square with 
	    vertices at the points (1, 0), (0, 1), (-1, 0), and (0, -1) by reflecting across the x-axis and across the line y = x.
	    Note that this is a faithful representation."},
	
	EXAMPLE {
	     "D = dihedralGroup 4",
	     "ms = matrix {{-1, 0}, {0, 1}}",
	     "mt = matrix {{0, 1}, {1, 0}}",
	     "f = map(D, {ms, mt})",
	     "ker f"
	     },
	    
	 SeeAlso => {(kernel, GroupMap), (image, GroupMap), (map, Group, Group), (map, Group, Group, List), toddCoxeterProcedure}
	    }
